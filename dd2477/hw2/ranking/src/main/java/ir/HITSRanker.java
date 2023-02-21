/**
 * Computes the Hubs and Authorities for an every document in a query-specific
 * link graph, induced by the base set of pages.
 *
 * @author Dmytro Kalpakchi
 */

package ir;

import java.util.*;
import java.io.*;


public class HITSRanker {

    /**
     * Max number of iterations for HITS
     */
    final static int MAX_NUMBER_OF_STEPS = 1000;

    /**
     * Convergence criterion: hub and authority scores do not
     * change more that EPSILON from one iteration to another.
     */
    final static double EPSILON = 0.001;

    /**
     * The inverted index
     */
    Index index;

    /**
     * Mapping from the titles to internal document ids used in the links file
     */
    HashMap<String, Integer> titleToId = new HashMap<>();

    /**
     * Sparse vector containing hub scores
     */
    HashMap<Integer, Double> hubs = new HashMap<>();

    /**
     * Sparse vector containing authority scores
     */
    HashMap<Integer, Double> authorities = new HashMap<>();


    /* --------------------------------------------- */

    HashMap<Integer, HashSet<Integer>> allInLinks = new HashMap<>();
    HashMap<Integer, HashSet<Integer>> allOutLinks = new HashMap<>();

    /**
     * Constructs the HITSRanker object
     * <p>
     * A set of linked documents can be presented as a graph.
     * Each page is a node in graph with a distinct nodeID associated with it.
     * There is an edge between two nodes if there is a link between two pages.
     * <p>
     * Each line in the links file has the following format:
     * nodeID;outNodeID1,outNodeID2,...,outNodeIDK
     * This means that there are edges between nodeID and outNodeIDi, where i is between 1 and K.
     * <p>
     * Each line in the titles file has the following format:
     * nodeID;pageTitle
     * <p>
     * NOTE: nodeIDs are consistent between these two files, but they are NOT the same
     * as docIDs used by search engine's Indexer
     *
     * @param linksFilename  File containing the links of the graph
     * @param titlesFilename File containing the mapping between nodeIDs and pages titles
     * @param index          The inverted index
     */
    public HITSRanker(String linksFilename, String titlesFilename, Index index) {
        this.index = index;
        readDocs(linksFilename, titlesFilename);
    }


    /* --------------------------------------------- */

    /**
     * A utility function that gets a file name given its path.
     * For example, given the path "davisWiki/hello.f",
     * the function will return "hello.f".
     *
     * @param path The file path
     * @return The file name.
     */
    private String getFileName(String path) {
        String result = "";
        StringTokenizer tok = new StringTokenizer(path, "\\/");
        while (tok.hasMoreTokens()) {
            result = tok.nextToken();
        }
        return result;
    }


    /**
     * Reads the files describing the graph of the given set of pages.
     *
     * @param linksFilename  File containing the links of the graph
     * @param titlesFilename File containing the mapping between nodeIDs and pages titles
     */
    void readDocs(String linksFilename, String titlesFilename) {
        //
        // YOUR CODE HERE
        //
        try (var reader = new BufferedReader(new FileReader(linksFilename))) {

            var line = reader.readLine();
            while (line != null) {
                var splits = line.split(";");

                var linkId = Integer.parseInt(splits[0]);

                hubs.put(linkId, 1.0);
                authorities.put(linkId, 1.0);

                var links = new HashSet<Integer>();

                if (splits.length > 1) {
                    for (var item : splits[1].split(",")) {
                        links.add(Integer.parseInt(item));
                    }
                }

                allOutLinks.put(linkId, links);

                for (var link : links) {
                    var inLinks = allInLinks.getOrDefault(link, new HashSet<>());
                    inLinks.add(linkId);
                    allInLinks.put(link, inLinks);
                }

                line = reader.readLine();
            }
        } catch (IOException ioException) {
            ioException.printStackTrace();
            System.exit(1);
        }


        try (var reader = new BufferedReader(new FileReader(titlesFilename))) {

            var line = reader.readLine();
            while (line != null) {
                var splits = line.split(";");
                titleToId.put(splits[1], Integer.parseInt(splits[0]));
                line = reader.readLine();
            }
        } catch (IOException ioException) {
            ioException.printStackTrace();
            System.exit(1);
        }
    }

    /**
     * Perform HITS iterations until convergence
     *
     * @param titles The titles of the documents in the root set
     */
    private void iterate(String[] titles) {
        //
        // YOUR CODE HERE
        //

        for (int i = 0; i < MAX_NUMBER_OF_STEPS; i++) {
            var newAuthorities = new HashMap<Integer, Double>();
            var newHubs = new HashMap<Integer, Double>();

            for (var authority : authorities.entrySet()) {
                var aId = authority.getKey();
                newAuthorities.put(aId, 0.0);

                var inLinks = allInLinks.get(aId);
                if (inLinks != null) {
                    for (var link : allInLinks.get(aId)) {
                        var hVal = hubs.get(link);
                        newAuthorities.put(aId, newAuthorities.getOrDefault(aId, 0.0) + hVal);
                    }
                }
            }

            for (var hub : hubs.entrySet()) {
                var hId = hub.getKey();
                newHubs.put(hId, 0.0);

                var outLinks = allOutLinks.get(hId);
                if (outLinks != null) {
                    for (var link : outLinks) {
                        var aVal = authorities.get(link);
                        newHubs.put(hId, newHubs.getOrDefault(hId, 0.0) + aVal);
                    }
                }
            }

            // Normalize
            var aScoreSumSq = 0.0;
            var hScoreSumSq = 0.0;

            for (var aVal : newAuthorities.values()) {
                aScoreSumSq += Math.pow(aVal, 2.0);
            }

            for (var hVal : newHubs.values()) {
                hScoreSumSq += Math.pow(hVal, 2.0);
            }

            var aNorm = Math.sqrt(aScoreSumSq);
            var hNorm = Math.sqrt(hScoreSumSq);

            newAuthorities.replaceAll((k, v) -> newAuthorities.get(k) / aNorm);
            newHubs.replaceAll((k, v) -> newHubs.get(k) / hNorm);


            var converged = converged(newAuthorities, newHubs, authorities, hubs);
            if (converged) {
                break;
            }

            authorities = newAuthorities;
            hubs = newHubs;
        }
    }

    private static boolean converged(HashMap<Integer, Double> authorities, HashMap<Integer, Double> hubs, HashMap<Integer, Double> prevAuthorities, HashMap<Integer, Double> prevHubs) {
        var sum = 0.0;

        for (var authority : authorities.entrySet()) {
            sum += Math.abs(authority.getValue() - prevAuthorities.get(authority.getKey()));
        }

        for (var hub : hubs.entrySet()) {
            sum += Math.abs(hub.getValue() - prevHubs.get(hub.getKey()));
        }

        System.out.println("diff: " + sum);

        return sum < EPSILON;
    }


    /**
     * Rank the documents in the subgraph induced by the documents present
     * in the postings list `post`.
     *
     * @param post The list of postings fulfilling a certain information need
     * @return A list of postings ranked according to the hub and authority scores.
     */
    PostingsList rank(PostingsList post) {
        //
        // YOUR CODE HERE
        //

        return null;
    }


    /**
     * Sort a hash map by values in the descending order
     *
     * @param map A hash map to sorted
     * @return A hash map sorted by values
     */
    private HashMap<Integer, Double> sortHashMapByValue(HashMap<Integer, Double> map) {
        if (map == null) {
            return null;
        } else {
            List<Map.Entry<Integer, Double>> list = new ArrayList<Map.Entry<Integer, Double>>(map.entrySet());

            list.sort((o1, o2) -> (o2.getValue()).compareTo(o1.getValue()));

            HashMap<Integer, Double> res = new LinkedHashMap<Integer, Double>();
            for (Map.Entry<Integer, Double> el : list) {
                res.put(el.getKey(), el.getValue());
            }
            return res;
        }
    }


    /**
     * Write the first `k` entries of a hash map `map` to the file `fname`.
     *
     * @param map   A hash map
     * @param fname The filename
     * @param k     A number of entries to write
     */
    void writeToFile(HashMap<Integer, Double> map, String fname, int k) {
        try {
            BufferedWriter writer = new BufferedWriter(new FileWriter(fname));

            if (map != null) {
                int i = 0;
                for (Map.Entry<Integer, Double> e : map.entrySet()) {
                    i++;
                    writer.write(e.getKey() + ": " + String.format("%.5g%n", e.getValue()));
                    if (i >= k) break;
                }
            }
            writer.close();
        } catch (IOException e) {

        }
    }


    /**
     * Rank all the documents in the links file. Produces two files:
     * hubs_top_30.txt with documents containing top 30 hub scores
     * authorities_top_30.txt with documents containing top 30 authority scores
     */
    void rank() {
        iterate(titleToId.keySet().toArray(new String[0]));
        HashMap<Integer, Double> sortedHubs = sortHashMapByValue(hubs);
        HashMap<Integer, Double> sortedAuthorities = sortHashMapByValue(authorities);
        writeToFile(sortedHubs, "hubs_top_30.txt", 30);
        writeToFile(sortedAuthorities, "authorities_top_30.txt", 30);
    }


    /* --------------------------------------------- */


    public static void main(String[] args) {
        if (args.length != 2) {
            System.err.println("Please give the names of the link and title files");
        } else {
            HITSRanker hr = new HITSRanker(args[0], args[1], null);
            hr.rank();
        }
    }
} 