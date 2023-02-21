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

	int noOfDocs;

	final static boolean doNormalPagerank = false;

	boolean isWikipedia;

	final static boolean writeData = true;
	final static boolean writeResult = true;

	final static double[] pagerank30 = new double[30];

	public class MonteCarloResult {
		double diff;
		long ms;
		long N;
		int[] sortedIndices;

		public MonteCarloResult(double diff, long ms, long N, int[] sortedIndices) {
			this.diff = diff;
			this.ms = ms;
			this.N = N;
			this.sortedIndices = sortedIndices;
		}
	}

	public PageRank(String filename) {
		isWikipedia = filename.contains("linksSvwiki");

		noOfDocs = readDocs(filename);
		readTitles();

		if (doNormalPagerank) {
			iterate(noOfDocs, 1000);
		} else {

			// load page rank
			try (var reader = new BufferedReader(new FileReader("grade-e/30-pagerank"))) {

				var line = reader.readLine();
				int index = 0;
				while (line != null) {
					var split = line.split(";");
					pagerank30[index++] = Double.parseDouble(split[1]);
					line = reader.readLine();
				}

			} catch (IOException io) {
				io.printStackTrace();
				System.exit(1);
			}

			if (isWikipedia) {
				runMonteCarloWikipedia();
			} else {
				long[] options = new long[] {
						noOfDocs / 25,
						noOfDocs / 5,
						noOfDocs,
						noOfDocs * 5,
						noOfDocs * 25,
						noOfDocs * 125,
						noOfDocs * 625,
				};
				runAllMonteCarloDavis(options);
			}

		}
	}

	void runAllMonteCarloDavis(long[] options) {
		var results = new MonteCarloResult[4][];
		for (int i = 0; i < results.length; i++) {
			results[i] = new MonteCarloResult[options.length];
		}

		for (int i = 0; i < options.length; i++) {
			var N = options[i];

			System.out.println("\nstarting monte carlo round with N: " + N);

			var res1 = monteCarlo1(noOfDocs, N);
			var res2 = monteCarlo2(noOfDocs, N);
			var res3 = monteCarlo4(noOfDocs, N);
			var res4 = monteCarlo5(noOfDocs, N);

			results[0][i] = res1;
			results[1][i] = res2;
			results[2][i] = res3;
			results[3][i] = res4;
		}

		for (int i = 0; i < 4; i++) {
			var monteCarloId = i < 2 ? i + 1 : i + 2;
			writeDavisResult(monteCarloId, results[i]);
		}
	}

	void runMonteCarloWikipedia() {
		monteCarlo5Wikipedia(noOfDocs);
	}

	boolean sameUntilIndex(int[] a1, int[] a2, int index) {
		for (int i = 0; i < index; i++) {
			if (a1[i] != a2[i]) {
				System.out.println("    was same until (exluding): " + i);
				return false;
			}
		}
		return true;
	}

	void writeDavisResult(int monteCarloId, MonteCarloResult[] results) {
		if (!writeResult) {
			return;
		}

		try (var writer = new BufferedWriter(new FileWriter("grade-b/result-" + monteCarloId))) {
			for (var result : results) {
				writer.write("" + result.N + ";" + result.diff + "\n");
			}
		} catch (IOException ioException) {
			ioException.printStackTrace();
			System.exit(1);
		}
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
		var titles = isWikipedia ? "svwikiTitles.txt" : "davisTitles.txt";

		try (BufferedReader in = new BufferedReader(new FileReader(titles))) {
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
		System.out.println("Starting normal pagerank algorithm");

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

		var largest30 = new double[30];
		var largest = sortIndices(prob);
		for (int i = 0; i < 30; i++) {
			largest30[i] = prob[largest[i]];
		}

		var then = System.currentTimeMillis();

		System.out.println("\ntook: " + new DecimalFormat("0.00").format((double) (then - now) / 1000.0) + " seconds");

		writeData(prob, "grade-e/pagerank");
		writeData(largest30, "grade-e/30-pagerank");
	}

	double getDiff(double[] largest30) {
		var diff = 0.0;
		for (int i = 0; i < 30; i++) {
			diff += Math.pow(pagerank30[i] - largest30[i], 2);
		}
		return diff;
	}

	MonteCarloResult monteCarlo1(int numberOfDocs, long N) {
		System.out.println("Starting monte carlo 1 for N: " + N);

		var now = System.currentTimeMillis();

		var randomizer = new Random();

		var counter = new double[numberOfDocs];

		// simulate N walks
		for (long i = 0; i < N; i++) {
			var docId = randomizer.nextInt(numberOfDocs);

			while (randomizer.nextDouble(1.0) > BORED) {
				var neighbors = link.get(docId);
				if (neighbors == null) {
					docId = randomizer.nextInt(numberOfDocs);
					continue;
				}
				var outlinks = neighbors.keySet();
				docId = (int) outlinks.toArray()[randomizer.nextInt(outlinks.size())];
			}

			counter[docId]++;
		}

		for (int i = 0; i < counter.length; i++) {
			counter[i] /= (double) N;
		}

		var then = System.currentTimeMillis();
		System.out.println("took: " + new DecimalFormat("0.00").format((double) (then - now) / 1000.0) + " seconds");

		var largest30 = new double[30];
		var largest = sortIndices(counter);
		for (int i = 0; i < 30; i++) {
			largest30[i] = counter[largest[i]];
		}

		writeData(counter, "grade-b/montecarlo1-" + N);
		writeData(largest30, "grade-b/30-montecarlo1-" + N);

		var diff = getDiff(largest30);

		return new MonteCarloResult(diff, then - now, N, largest);
	}

	MonteCarloResult monteCarlo2(int numberOfDocs, long N) {
		System.out.println("Starting monte carlo 2 for N: " + N);
		var now = System.currentTimeMillis();

		var randomizer = new Random();
		var counter = new double[numberOfDocs];

		// simulate N walks
		for (long i = 0; i < N; i++) {
			var docId = (int) (i % numberOfDocs);

			while (randomizer.nextDouble(1.0) > BORED) {
				var neighbors = link.get(docId);
				if (neighbors == null) {
					docId = randomizer.nextInt(numberOfDocs);
					continue;
				}
				var outlinks = neighbors.keySet();
				docId = (int) outlinks.toArray()[randomizer.nextInt(outlinks.size())];
			}

			counter[docId]++;
		}

		for (int i = 0; i < counter.length; i++) {
			counter[i] /= (double) N;
		}

		var then = System.currentTimeMillis();
		System.out.println("took: " + new DecimalFormat("0.00").format((double) (then - now) / 1000.0) + " seconds");

		var largest30 = new double[30];
		var largest = sortIndices(counter);
		for (int i = 0; i < 30; i++) {
			largest30[i] = counter[largest[i]];
		}

		writeData(counter, "grade-b/montecarlo2-" + N);
		writeData(largest30, "grade-b/30-montecarlo2-" + N);

		var diff = getDiff(largest30);

		return new MonteCarloResult(diff, then - now, N, largest);
	}

	MonteCarloResult monteCarlo4(int numberOfDocs, long N) {
		System.out.println("Starting monte carlo 4 for N: " + N);
		var now = System.currentTimeMillis();

		var randomizer = new Random();
		var counter = new double[noOfDocs];
		var totalVisits = 0;

		// simulate N walks
		for (long i = 0; i < N; i++) {
			var docId = (int) (i % numberOfDocs);

			while (randomizer.nextDouble(1.0) > BORED) {
				// count every node along the path
				counter[docId]++;
				totalVisits++;

				var neighbors = link.get(docId);
				if (neighbors == null) {
					break;
				}
				var outlinks = neighbors.keySet();
				docId = (int) outlinks.toArray()[randomizer.nextInt(outlinks.size())];
			}

		}

		for (int i = 0; i < counter.length; i++) {
			counter[i] /= (double) totalVisits;
		}

		var then = System.currentTimeMillis();
		System.out.println("took: " + new DecimalFormat("0.00").format((double) (then - now) / 1000.0) + " seconds");

		var largest30 = new double[30];
		var largest = sortIndices(counter);
		for (int i = 0; i < 30; i++) {
			largest30[i] = counter[largest[i]];
		}

		writeData(counter, "grade-b/montecarlo4-" + N);
		writeData(largest30, "grade-b/30-montecarlo4-" + N);

		var diff = getDiff(largest30);

		return new MonteCarloResult(diff, then - now, N, largest);
	}

	MonteCarloResult monteCarlo5(int numberOfDocs, long N) {
		System.out.println("Starting monte carlo 5 for N: " + N);
		var now = System.currentTimeMillis();

		var randomizer = new Random();
		var counter = new double[numberOfDocs];

		var totalVisits = 0;

		// simulate N walks
		for (long i = 0; i < N; i++) {
			var docId = randomizer.nextInt(numberOfDocs);

			while (randomizer.nextDouble(1.0) > BORED) {
				// count every node along the path
				counter[docId]++;
				totalVisits++;

				var neighbors = link.get(docId);
				if (neighbors == null) {
					break;
				}
				var outlinks = neighbors.keySet();
				docId = (int) outlinks.toArray()[randomizer.nextInt(outlinks.size())];
			}

		}

		for (int i = 0; i < counter.length; i++) {
			counter[i] /= totalVisits;
		}

		var then = System.currentTimeMillis();
		System.out.println("took: " + new DecimalFormat("0.00").format((double) (then - now) / 1000.0) + " seconds");

		var largest30 = new double[30];
		var largest = sortIndices(counter);
		for (int i = 0; i < 30; i++) {
			largest30[i] = counter[largest[i]];
		}

		writeData(counter, "grade-b/montecarlo5-" + N);
		writeData(largest30, "grade-b/30-montecarlo5-" + N);

		var diff = getDiff(largest30);

		return new MonteCarloResult(diff, then - now, N, largest);
	}

	class StableCheckResult {
		boolean stable;
		int[] indices;
		double[] values;

		public StableCheckResult(boolean stable, int[] largestIndices, double[] largest30) {
			this.stable = stable;
			this.indices = largestIndices;
			this.values = largest30;
		}
	}

	void monteCarlo5Wikipedia(int numberOfDocs) {
		System.out.println("Starting monte carlo 5 (wikipedia)");
		var now = System.currentTimeMillis();

		var randomizer = new Random();
		var counter = new double[numberOfDocs];

		var totalVisits = 0;
		var stableCounter = 0;

		int[] lastSortedIndices = null;

		for (int i = 0;; i++) {
			var docId = randomizer.nextInt(numberOfDocs);

			while (randomizer.nextDouble(1.0) > BORED) {
				counter[docId]++;
				totalVisits++;

				var neighbors = link.get(docId);
				if (neighbors == null) {
					break;
				}
				var outlinks = neighbors.keySet();
				docId = (int) outlinks.toArray()[randomizer.nextInt(outlinks.size())];
			}

			if (i % 10000 == 0) {
				System.out.println("  checking if wikipedia is stable at i=" + i);

				var result = checkIfStable(counter, totalVisits, lastSortedIndices);
				if (result.stable) {
					System.out.println("Wikipedia seems to be stable");
					if (++stableCounter > 4) {

						try (var writer = new BufferedWriter(new FileWriter("grade-b/wiki-30-montecarlo5"))) {
							for (int j = 0; j < 30; j++) {
								writer.write(docTitles[Integer.parseInt(docName[result.indices[j]])] + ";"
										+ result.values[j] + "\n");
							}
						} catch (Exception e) {
							System.out.println("failed to write to file: " + e.getMessage());
							System.out.println(e.getStackTrace());
						}

						break;
					}
				} else {
					System.out.println("Wikipedia is NOT stable");
					stableCounter = 0;
				}
				lastSortedIndices = result.indices;
			}

		}

		for (int i = 0; i < counter.length; i++) {
			counter[i] /= totalVisits;
		}

		var then = System.currentTimeMillis();
		System.out.println("took: " + new DecimalFormat("0.00").format((double) (then - now) / 1000.0) + " seconds");

	}

	void writeData(double[] values, String file) {
		if (!writeData) {
			return;
		}

		try (var writer = new BufferedWriter(new FileWriter(file))) {
			for (int i = 0; i < values.length; i++) {
				writer.write(docTitles[Integer.parseInt(docName[i])] + ";" + values[i] + "\n");
			}
		} catch (Exception e) {
			System.out.println("failed to write to file: " + e.getMessage());
			System.out.println(e.getStackTrace());
		}
	}

	StableCheckResult checkIfStable(double[] counter, int totalVisits, int[] lastSortedIndices) {
		var scores = Arrays.copyOf(counter, counter.length);

		for (int i = 0; i < scores.length; i++) {
			scores[i] /= totalVisits;
		}

		var largest30 = new double[30];
		var largestIndices = sortIndices(scores);
		for (int j = 0; j < 30; j++) {
			largest30[j] = scores[largestIndices[j]];
		}

		if (lastSortedIndices != null) {
			var sameFirst30 = sameUntilIndex(largestIndices, lastSortedIndices, 30);
			return new StableCheckResult(sameFirst30, largestIndices, largest30);
		}

		return new StableCheckResult(false, largestIndices, largest30);
	}

	int[] sortIndices(double[] arr) {
		var indices = new Integer[arr.length];
		for (int i = 0; i < arr.length; i++) {
			indices[i] = i;
		}

		Arrays.sort(indices, (i1, i2) -> Double.compare(arr[i2], arr[i1]));

		return Arrays.stream(indices).mapToInt(i -> i).toArray();
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