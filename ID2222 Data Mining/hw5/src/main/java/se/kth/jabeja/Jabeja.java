package se.kth.jabeja;

import org.apache.log4j.Logger;
import se.kth.jabeja.config.Config;
import se.kth.jabeja.config.NodeSelectionPolicy;
import se.kth.jabeja.io.FileIO;
import se.kth.jabeja.rand.RandNoGenerator;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Random;

public class Jabeja {
    final static Logger logger = Logger.getLogger(Jabeja.class);
    private final Config config;
    private final HashMap<Integer/*id*/, Node/*neighbors*/> entireGraph;
    private final List<Integer> nodeIds;
    private int numberOfSwaps;
    private int round;
    private float T;
    private boolean resultFileCreated = false;
    private final int acceptancePolicy = 1;

    //-------------------------------------------------------------------
    public Jabeja(HashMap<Integer, Node> graph, Config config) {
        this.entireGraph = graph;
        this.nodeIds = new ArrayList<>(entireGraph.keySet());
        this.round = 0;
        this.numberOfSwaps = 0;
        this.config = config;
        this.T = config.getTemperature();
    }


    private double getAcceptanceProbability(double oldCost, double newCost) {
        if (acceptancePolicy == 1) {
            return Math.pow(Math.E, (newCost - oldCost) / T);
        }
        return newCost * T > oldCost ? 1 : 0;
    }


    //-------------------------------------------------------------------
    public void startJabeja() throws IOException {
        for (round = 0; round < config.getRounds(); round++) {
            for (int id : entireGraph.keySet()) {
                sampleAndSwap(id);
            }

            //one cycle for all nodes have completed.
            //reduce the temperature
            saCoolDown();
            report();
        }
    }

    /**
     * Simulated annealing cooling function
     */
    private void saCoolDown() {
        // TODO for second task
        if (T > 1)
            T -= config.getDelta();
        if (T < 1)
            T = 1;
    }

    /**
     * Sample and swap algorithm at node p
     */
    private void sampleAndSwap(int nodeId) {
        Node partner = null;
        Node node = entireGraph.get(nodeId);

        if (config.getNodeSelectionPolicy() != NodeSelectionPolicy.HYBRID) {
            // swap with random neighbors
            partner = findPartner(node.getId(), getNeighbors(node));
        }

        if (config.getNodeSelectionPolicy() == NodeSelectionPolicy.HYBRID
                || config.getNodeSelectionPolicy() == NodeSelectionPolicy.RANDOM) {
            // if local policy fails then randomly sample the entire graph
            partner = findPartner(node.getId(), getSample(node.getId()));
        }

        if (partner == null)
            return;

        // swap the colors
        var tmp = node.getColor();
        node.setColor(partner.getColor());
        partner.setColor(tmp);
        numberOfSwaps++;
    }

    public Node findPartner(int nodeId, Integer[] nodes) {

        Node nodep = entireGraph.get(nodeId);

        Node bestPartner = null;
        double highestBenefit = 0;

        for (var node : nodes) {
            var candidate = entireGraph.get(node);

            double alpha = 2.0f;

            var degree = Math.pow(getDegree(nodep, candidate.getColor()), alpha) + Math.pow(getDegree(candidate, nodep.getColor()), alpha);
            var current = Math.pow(getDegree(nodep, nodep.getColor()), alpha) + Math.pow(getDegree(candidate, candidate.getColor()), alpha);

            if (degree > highestBenefit && getAcceptanceProbability(current, degree) > new Random().nextDouble()) {
                highestBenefit = degree;
                bestPartner = candidate;
            }
        }

        return bestPartner;
    }

    /**
     * The degree on the node based on color
     *
     * @return how many neighbors of the node have color == colorId
     */
    private int getDegree(Node node, int colorId) {
        int degree = 0;
        for (int neighborId : node.getNeighbours()) {
            Node neighbor = entireGraph.get(neighborId);
            if (neighbor.getColor() == colorId) {
                degree++;
            }
        }
        return degree;
    }

    /**
     * Returns a uniformly random sample of the graph
     *
     * @return Returns a uniformly random sample of the graph
     */
    private Integer[] getSample(int currentNodeId) {
        int count = config.getUniformRandomSampleSize();
        int rndId;
        int size = entireGraph.size();
        ArrayList<Integer> rndIds = new ArrayList<>();

        do {
            rndId = nodeIds.get(RandNoGenerator.nextInt(size));
            if (rndId != currentNodeId && !rndIds.contains(rndId)) {
                rndIds.add(rndId);
                count--;
            }

        } while (count != 0);

        Integer[] ids = new Integer[rndIds.size()];
        return rndIds.toArray(ids);
    }

    /**
     * Get random neighbors. The number of random neighbors is controlled using
     * -closeByNeighbors command line argument which can be obtained from the config
     * using {@link Config#getRandomNeighborSampleSize()}
     */
    private Integer[] getNeighbors(Node node) {
        ArrayList<Integer> list = node.getNeighbours();
        int count = config.getRandomNeighborSampleSize();
        int rndId;
        int index;
        int size = list.size();
        ArrayList<Integer> rndIds = new ArrayList<>();

        if (size <= count)
            rndIds.addAll(list);
        else {
            do {
                index = RandNoGenerator.nextInt(size);
                rndId = list.get(index);
                if (!rndIds.contains(rndId)) {
                    rndIds.add(rndId);
                    count--;
                }

            } while (count != 0);
        }

        Integer[] arr = new Integer[rndIds.size()];
        return rndIds.toArray(arr);
    }


    /**
     * Generate a report which is stored in a file in the output dir.
     */
    private void report() throws IOException {
        int grayLinks = 0;
        int migrations = 0; // number of nodes that have changed the initial color
        int size = entireGraph.size();

        for (int i : entireGraph.keySet()) {
            Node node = entireGraph.get(i);
            int nodeColor = node.getColor();
            ArrayList<Integer> nodeNeighbours = node.getNeighbours();

            if (nodeColor != node.getInitColor()) {
                migrations++;
            }

            if (nodeNeighbours != null) {
                for (int n : nodeNeighbours) {
                    Node p = entireGraph.get(n);
                    int pColor = p.getColor();

                    if (nodeColor != pColor)
                        grayLinks++;
                }
            }
        }

        int edgeCut = grayLinks / 2;

        logger.info("round: " + round +
                ", edge cut:" + edgeCut +
                ", swaps: " + numberOfSwaps +
                ", migrations: " + migrations);

        saveToFile(edgeCut, migrations);
    }

    private void saveToFile(int edgeCuts, int migrations) throws IOException {
        String delimiter = "\t\t";
        String outputFilePath;

        //output file name
        File inputFile = new File(config.getGraphFilePath());
        outputFilePath = config.getOutputDir() +
                File.separator +
                inputFile.getName() + "_" +
                "NS" + "_" + config.getNodeSelectionPolicy() + "_" +
                "GICP" + "_" + config.getGraphInitialColorPolicy() + "_" +
                "T" + "_" + config.getTemperature() + "_" +
                "D" + "_" + config.getDelta() + "_" +
                "RNSS" + "_" + config.getRandomNeighborSampleSize() + "_" +
                "URSS" + "_" + config.getUniformRandomSampleSize() + "_" +
                "A" + "_" + config.getAlpha() + "_" +
                "R" + "_" + config.getRounds() + ".txt";

        if (!resultFileCreated) {
            File outputDir = new File(config.getOutputDir());
            if (!outputDir.exists()) {
                if (!outputDir.mkdir()) {
                    throw new IOException("Unable to create the output directory");
                }
            }
            // create folder and result file with header
            String header = "# Migration is number of nodes that have changed color.";
            header += "\n\nRound" + delimiter + "Edge-Cut" + delimiter + "Swaps" + delimiter + "Migrations" + delimiter + "Skipped" + "\n";
            FileIO.write(header, outputFilePath);
            resultFileCreated = true;
        }

        FileIO.append(round + delimiter + (edgeCuts) + delimiter + numberOfSwaps + delimiter + migrations + "\n", outputFilePath);
    }
}
