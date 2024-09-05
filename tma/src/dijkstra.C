#include "util.h"
#include "tma.h"
#include "rnf.h"
#include "dnet.h"

//**************************************************************************
//
//  Dijkstra's algorithm.
//  Performs the signal propagation, using a variant of
//  Dijkstra's algorithm (aka the Bathtub Algorithm).
//  Produces bestTime, and flowFrom fields in net.
//
//**************************************************************************
int dijkstra(int startLine, int startSample,
             int endLine,   int endSample,   int endPointSet,
             dnet *dijkstraNet, SubRegion &aoi) {
  //int lines = aoi.getLines();
  int samples = aoi.getSamples();
  double compare, minDry;
  int candidate, minDryCandidate;
  int neighbor, neighborNode;

//    FILE * tmaLog = fopen ("/tmp/tma.log", "a");
//    fprintf (tmaLog, "dijkstra (startLine = %d, startSample = %d, endLine = %d, endSample = %d, endPointSet = %d\n",
//  	   startLine, startSample, endLine, endSample, endPointSet);
//      fclose (tmaLog);

  // Initialize.

  dnetNode *net = dijkstraNet->nv;
  int node;
  for (node=0; node<dijkstraNet->nc; node++) {
    net[node].bestTime = TOO_HUGE;
    net[node].alreadyWet = 0;
    net[node].withinBorder = 0;   // Speed up.
  }

  int startNode = startSample + startLine * samples;
  int endNode = endSample + endLine * samples;

  cerr << "startNode was " << startNode << endl;
  if (preferRoad (dijkstraNet, & startNode))
    cerr << "preferRoad not happy\n";
  else
    cerr << "preferRoad happy\n";
  cerr << "startNode is " << startNode << endl;

  net[startNode].bestTime = 0;
  net[startNode].alreadyWet = 1;
  // This will terminate the flow back in tracePath.
  net[startNode].flowFrom = -1;
  net[startNode].withinBorder = 1;

  // Initialize candidate list for speed up.

  int numCandidates = net[startNode].nbc;
  int placesInCandidateList = numCandidates;
  int *candidateList = (int *) malloc(sizeof(int) * placesInCandidateList);

  // Initial candidates are neighbors of the start point.
  for (node=0; node<numCandidates; node++) {
    neighborNode = net[startNode].nbv[node];
    candidateList[node] = neighborNode;
    net[neighborNode].withinBorder = 1;
  }

  int latestWetNode = startNode;

//   tmaLog = fopen ("/tmp/tma.log", "a");
//      fprintf (tmaLog, "dijkstra starting the do...\n");
//      fclose (tmaLog);

  do {

    //
    //   For every pass, computes minimum time of one more location.
    //   If endPointSet is set then stop when reach the end point.
    //
    //   Check each of the adjacent locations to see if
    //   they could be reached faster by going through
    //   latestWetNode.
    //

    // Update times.

    for (neighbor=0; neighbor<net[latestWetNode].nbc; neighbor++) {

      neighborNode = net[latestWetNode].nbv[neighbor];

      if (!net[neighborNode].alreadyWet) {

	compare = net[latestWetNode].bestTime +
	  net[latestWetNode].nbcv[neighbor];

	if (compare < net[neighborNode].bestTime) {
	  net[neighborNode].bestTime = compare;
	  net[neighborNode].flowFrom = latestWetNode;
	}
      }
    }

    //
    //   Find location with smallest time amoung
    //   those that have not been labeled yet.
    //   Set latestWetNode and
    //   set alreadyWet.
    //
    //   Only check the candidate list.
    //

    minDry = TOO_HUGE + 1;
    for (candidate=0; candidate<numCandidates; candidate++)
      if (net[candidateList[candidate]].bestTime < minDry) {
	// Save it.
	minDry = net[candidateList[candidate]].bestTime;
	minDryCandidate = candidate;
      }

    // Wet the node.

    latestWetNode = candidateList[minDryCandidate];
    net[latestWetNode].alreadyWet = 1;

//      tmaLog = fopen ("/tmp/tma.log", "a");
//      fprintf (tmaLog, "dijkstra just wet line = %d, sample = %d\n",
//  	     IDX2LINE(net[latestWetNode].gpIndex,dijkstraNet), IDX2SAMPLE(net[latestWetNode].gpIndex,dijkstraNet));
//      fclose (tmaLog);


    // Remove from candidate list.

    candidateList[minDryCandidate] = candidateList[numCandidates-1];
    numCandidates--;
  
    // Check neighbors.
    // If not within border, add to candidate list, and set within border.
    //fprintf(stderr, "num neighbors for %d are %d\n", latestWetNode, net[latestWetNode].nbc);
    // Realloc.
    if (numCandidates + net[latestWetNode].nbc > placesInCandidateList) {
      placesInCandidateList += 256;
      candidateList  = (int *) realloc(candidateList,  sizeof(int) * placesInCandidateList);
    }

    for (neighbor=0; neighbor<net[latestWetNode].nbc; neighbor++) {

      neighborNode = net[latestWetNode].nbv[neighbor];

      if (!net[neighborNode].withinBorder) {
	candidateList[numCandidates] = neighborNode;
	numCandidates++;
	net[neighborNode].withinBorder = 1;
      }
    }

    if (endPointSet)
      if (latestWetNode == endNode)
	break;

  } while (numCandidates > 0);

//    tmaLog = fopen ("/tmp/tma.log", "a");
//    fprintf (tmaLog, "dijkstra done the do...\n");
//    fclose (tmaLog);

  free(candidateList);

  return (1);
  // th'end of dijkstra.
}


//minTimeSoFar = doubleMatrix(lines, samples);
