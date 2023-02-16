import java.util.*;
import java.io.*;
import java.text.DecimalFormat;

public class PageRank {

	/**
	 * Maximal number of documents. We're assuming here that we
	 * don't have more docs than we can keep in main memory.
	 */
	final static int MAX_NUMBER_OF_DOCS = 2000000;

	/**
	 * Mapping from document names to document numbers.
	 */
	HashMap<String, Integer> docNumber = new HashMap<String, Integer>();

	/**
	 * Mapping from document numbers to document names
	 */
	String[] docName = new String[MAX_NUMBER_OF_DOCS];
	String[] docTitles = new String[MAX_NUMBER_OF_DOCS];

	/**
	 * A memory-efficient representation of the transition matrix.
	 * The outlinks are represented as a HashMap, whose keys are
	 * the numbers of the documents linked from.
	 * <p>
	 *
	 * The value corresponding to key i is a HashMap whose keys are
	 * all the numbers of documents j that i links to.
	 * <p>
	 *
	 * If there are no outlinks from i, then the value corresponding
	 * key i is null.
	 */
	HashMap<Integer, HashMap<Integer, Boolean>> link = new HashMap<Integer, HashMap<Integer, Boolean>>();

	/**
	 * The number of outlinks from each node.
	 */
	int[] out = new int[MAX_NUMBER_OF_DOCS];

	/**
	 * The probability that the surfer will be bored, stop
	 * following links, and take a random jump somewhere.
	 */
	final static double BORED = 0.15;
	final static double C = 1.0 - BORED;

	/**
	 * Convergence criterion: Transition probabilities do not
	 * change more that EPSILON from one iteration to another.
	 */
	final static double EPSILON = 0.0001;

	/* --------------------------------------------- */

	public PageRank(String filename) {
		int noOfDocs = readDocs(filename);
		readTitles();
		iterate(noOfDocs, 1000);
	}

	/* --------------------------------------------- */

	/**
	 * Reads the documents and fills the data structures.
	 *
	 * @return the number of documents read.
	 */
	int readDocs(String filename) {
		int fileIndex = 0;
		try {
			System.err.print("Reading file... ");
			BufferedReader in = new BufferedReader(new FileReader(filename));
			String line;
			while ((line = in.readLine()) != null && fileIndex < MAX_NUMBER_OF_DOCS) {
				int index = line.indexOf(";");
				String title = line.substring(0, index);
				Integer fromdoc = docNumber.get(title);
				// Have we seen this document before?
				if (fromdoc == null) {
					// This is a previously unseen doc, so add it to the table.
					fromdoc = fileIndex++;
					docNumber.put(title, fromdoc);
					docName[fromdoc] = title;
				}
				// Check all outlinks.
				StringTokenizer tok = new StringTokenizer(line.substring(index + 1), ",");
				while (tok.hasMoreTokens() && fileIndex < MAX_NUMBER_OF_DOCS) {
					String otherTitle = tok.nextToken();
					Integer otherDoc = docNumber.get(otherTitle);
					if (otherDoc == null) {
						// This is a previousy unseen doc, so add it to the table.
						otherDoc = fileIndex++;
						docNumber.put(otherTitle, otherDoc);
						docName[otherDoc] = otherTitle;
					}
					// Set the probability to 0 for now, to indicate that there is
					// a link from fromdoc to otherDoc.
					if (link.get(fromdoc) == null) {
						link.put(fromdoc, new HashMap<Integer, Boolean>());
					}
					if (link.get(fromdoc).get(otherDoc) == null) {
						link.get(fromdoc).put(otherDoc, true);
						out[fromdoc]++;
					}
				}
			}
			if (fileIndex >= MAX_NUMBER_OF_DOCS) {
				System.err.print("stopped reading since documents table is full. ");
			} else {
				System.err.print("done. ");
			}
		} catch (FileNotFoundException e) {
			System.err.println("File " + filename + " not found!");
		} catch (IOException e) {
			System.err.println("Error reading file " + filename);
		}
		System.err.println("Read " + fileIndex + " number of documents");
		return fileIndex;
	}

	/* --------------------------------------------- */

	void readTitles() {
		try (BufferedReader in = new BufferedReader(new FileReader("davisTitles.txt"))) {
			System.err.print("Reading file... ");

			var line = in.readLine();
			while (line != null) {
				var splits = line.split(";");
				docTitles[Integer.parseInt(splits[0])] = splits[1];
				line = in.readLine();
			}

		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/*
	 * Chooses a probability vector a, and repeatedly computes
	 * aP, aP^2, aP^3... until aP^i = aP^(i+1).
	 */
	void iterate(int numberOfDocs, int maxIterations) {
		var now = System.currentTimeMillis();

		var prob = new double[numberOfDocs];

		var probPrime = new double[numberOfDocs];
		probPrime[0] = 1.0;

		int prIteration = 0;
		while (arrayDiff(prob, probPrime) > EPSILON && prIteration < maxIterations) {

			for (int i = 0; i < numberOfDocs; i++) {
				prob[i] = probPrime[i];
				probPrime[i] = 0;
			}

			var jump = 1.0 / (double) numberOfDocs;

			for (int i = 0; i < numberOfDocs; i++) {
				var currentProb = prob[i];
				var outlinks = link.get(i);
				var noOutlinks = out[i];

				if (noOutlinks == 0) {
					for (int j = 0; j < numberOfDocs; j++) {
						probPrime[j] += jump * currentProb;
					}

					continue;
				}

				for (int j = 0; j < numberOfDocs; j++) {

					var hasLink = outlinks == null ? false : outlinks.getOrDefault(j, false);

					var link = hasLink ? 1.0 / (double) noOutlinks : 0.0;

					var contrib = C * link + (1.0 - C) * jump;

					probPrime[j] += currentProb * contrib;
				}

			}

			prIteration++;
		}

		var largest30 = sortIndices(prob);
		for (int i = 0; i < 30; i++) {
			System.out.println(docName[largest30[i]] + ": " + prob[largest30[i]]);
		}

		var then = System.currentTimeMillis();

		System.out.println("\ntook: " + new DecimalFormat("0.00").format((double) (then - now) / 1000.0) + " seconds");

		writeData(prob);
	}

	void writeData(double[] values) {
		try (var writer = new BufferedWriter(new FileWriter("grade-c/pagerank"))) {
			for (int i = 0; i < values.length; i++) {
				writer.write(docTitles[Integer.parseInt(docName[i])] + ";" + values[i] + "\n");
			}
		} catch (Exception e) {
			System.out.println("failed to write to file: " + e.getMessage());
			System.out.println(e.getStackTrace());
		}
	}

	Integer[] sortIndices(double[] arr) {
		var indices = new Integer[arr.length];
		for (int i = 0; i < arr.length; i++) {
			indices[i] = i;
		}

		Arrays.sort(indices, (i1, i2) -> Double.compare(arr[i2], arr[i1]));

		return indices;
	}

	double arrayDiff(double[] prob1, double[] prob2) {
		if (prob1.length != prob2.length) {
			System.err.println("bad array size");
			System.exit(1);
		}

		var diff = 0.0;

		for (int i = 0; i < prob1.length; i++) {
			diff += Math.abs(prob1[i] - prob2[i]);
		}

		return diff;
	}

	/* --------------------------------------------- */

	public static void main(String[] args) {
		if (args.length != 1) {
			System.err.println("Please give the name of the link file");
		} else {
			new PageRank(args[0]);
		}
	}
}