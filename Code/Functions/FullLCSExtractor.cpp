//
//  FullLCSExtractor.cpp
//  
//
//  Created by Nat Coombs on 10/4/21.
//
//  Adapted from the following sources: https://learnersbucket.com/examples/algorithms/longest-common-subsequence-print-all-lcs/
//  https://www.techiedelight.com/longest-common-subsequence-finding-lcs/
//  https://www.geeksforgeeks.org/print-longest-common-sub-sequences-lexicographical-order/
//  https://github.com/cran/qualV/blob/master/src/lcs.c

#include <Rcpp.h>
//# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#include <stdio.h>
#include <iostream>
#include <string.h>
#include <vector>
#include <cstring>
#include <algorithm>
#include <unordered_set>
#include <set>
#include <cmath>
#include <ctgmath>
#include <utility>
#include <numeric>
#include <list>
#include <queue>
#include <stack>
//int k = 0;



void IntLibConverter(std::vector<std::string> Fish1Seq_, std::vector<std::string> Fish2Seq_,
                     std::vector<int> &Fish1Seq, std::vector<int> &Fish2Seq,
                     std::vector<std::string> &LocLib){
// in here, we create a vector of all unique locations (LocLib), and from it we make new sequences of integers that key to it.
    LocLib.insert(LocLib.end(), Fish1Seq_.begin(), Fish1Seq_.end());
    LocLib.insert(LocLib.end(), Fish2Seq_.begin(), Fish2Seq_.end());
    
    std::set<std::string> s;
    for (std::string i:LocLib){
        s.insert(i);
    }
    LocLib.assign(s.begin(), s.end());
    sort(LocLib.begin(), LocLib.end());
    for(int i = 0; i < Fish1Seq_.size(); i++){
        auto ind = find(LocLib.begin(), LocLib.end(), Fish1Seq_[i]);
        if(ind != LocLib.end()){
            int Index = ind - LocLib.begin();
            Fish1Seq.push_back(Index);
        }
    }
    
    for(int i = 0; i < Fish2Seq_.size(); i++){
        auto ind = find(LocLib.begin(), LocLib.end(), Fish2Seq_[i]);
        if(ind != LocLib.end()){
            int Index = ind - LocLib.begin();
            Fish2Seq.push_back(Index);
        }
    }
}
void DPmatInit(int Size1, int Size2, std::vector<std::vector<int>> & DPmat){
         DPmat.clear();
         DPmat.resize(Size1 + 1, std::vector<int>(Size2+1,-1));
    

}

// Potential approach for creating the 3-D data structure for C++: http://www.cplusplus.com/forum/general/833/
//

// Need to create a version that uses ONLY C++/C code and objects, see Dirk's post on Fibonacci algs
int lcscore(std::vector<int> Fish1Seq, std::vector<int> Fish2Seq, int Size1, int Size2, int i, int j, std::vector<std::vector<int>> & DPmat){
    // Set the memory address to be tested and modified
    int & ret = DPmat[i][j];
    

    // Check to see if the run is done
    if (i==Size1 || j==Size2)
    {
        return ret = 0;
    }
    
    // Check to see if LCS is already computed
    if (ret != -1)
        {
            return ret;
        }
    
    ret = 0;

    bool matches = Fish1Seq[i] == Fish2Seq[j];
    
// if characters are same return previous + 1 else
    // max of two sequences after removing i'th and j'th
    // char one by one
        if (matches == true)
        {
       //     Rcout << "This should be returned on match" << "\n";

            ret = 1 + lcscore(Fish1Seq, Fish2Seq, Size1, Size2, i+1, j+1, DPmat);
        }
        else
        {
      //      Rcout << "This should be returned on unmatch" << "\n";

            ret = std::max(lcscore(Fish1Seq, Fish2Seq, Size1, Size2, i+1, j, DPmat),
                        lcscore(Fish1Seq, Fish2Seq, Size1, Size2, i, j+1, DPmat));
            
        }
     //  Rcout << "This should be returned unless it's done" << "\n";
        return ret;

}

void PullAll(std::vector<int> Fish1Seq, std::vector<int> Fish2Seq, int Size1, int Size2, std::vector<int> & data, int indx1, int indx2, int currlcs,std::vector<std::vector<int>> & LCSset, int lcslen, std::vector<std::vector<int>> DPmat/* std::vector<std::vector<std::vector<std::string>>> & PullSet, std::vector<std::string> &F1Index, std::vector<std::string> &F2Index*/){

    // if currlcs is equal to lcslen then staple it to the output
        if (currlcs == lcslen)
        {
            // Declare and clear the container for the LCS & constructor set
            /*std::vector<std::vector<std::string>> LCSset;*/
            
            // Stapling together the LCS & constructor set
            LCSset.push_back(data);
           /* LCSset.push_back(F1Index);
            LCSset.push_back(F2Index);
            
            // Stapling the set to the output
            PullSet.push_back(LCSset);*/

            return;
        }
     
        // if we are done with all the steps of both vectors
        if (indx1==Size1 || indx2==Size2)
        {return;}

    
    //Iterate as needed
    for (int i=indx1; i<Size1; i++)
            {
                for (int j=indx2; j<Size2; j++)
                              {
                                  if (Fish1Seq[i] == Fish2Seq[j] &&
                                                    DPmat[i][j] == lcslen-currlcs)
                                                  {
                                                    data[currlcs] = Fish1Seq[i];
                                                      //F1Index[currlcs] = std::to_string(i + 1);
                                                      //F2Index[currlcs] = std::to_string(j + 1);
                                                    PullAll(Fish1Seq, Fish2Seq, Size1, Size2, data, i+1, j+1, currlcs+1, LCSset, lcslen, DPmat/* PullSet, F1Index, F2Index*/);
                                                    break;
                                                  }
                                  
                              }
            }
}
//void LCSConstructStepper(std::vector<int> LCS, std::vector<std::vector<int>> FishMatchTable, std::vector<std::vector<int>> & ConstrSet, std::vector<int> & ConstrVec, int RunDex, int currlcs, int lcslen){
//    // This part is really slow due to the recursion, but I'm not sure how best to speed it up.
//    if(currlcs == lcslen){
//        ConstrSet.push_back(ConstrVec);
//        return;
//    }
//    // Remember that this is inside a for loop using i, but it shouldn't cause an issue to loop like this anyway
//    for(int j = 0; j<FishMatchTable[RunDex].size(); j++){
//        ConstrVec[RunDex] = FishMatchTable[RunDex][j];
//        if(RunDex == 0 || ConstrVec[RunDex] > ConstrVec[RunDex-1]){
//            LCSConstructStepper(LCS, FishMatchTable, ConstrSet, ConstrVec, RunDex + 1, currlcs + 1, lcslen);
//        }
//    }
//    return;
//}



std::vector<std::string> LCSStringify(std::vector<int> LCSInt, int lcslen, std::vector<std::string> LocLib){
    std::vector<std::string> LCSString;
    for(int k = 0; k < lcslen; k++){
        LCSString.push_back(LocLib[LCSInt[k]]);
    }
    return(LCSString);
}

std::vector<std::vector<int>> KeyFinder(std::vector<std::vector<int>> DKey){
    std::vector<std::vector<int>> Keys(2);
    std::vector<int> StartKeys;
    std::vector<int> EndKeys;
    int MaxDepth = *std::max_element(DKey[1].begin(),DKey[1].end());
    for(int i = 0; i < DKey[0].size(); i++){
        if(DKey[1][i] == 0){
            StartKeys.push_back(DKey[0][i]);
        } else {if(DKey[1][i] == MaxDepth){
            EndKeys.push_back(DKey[0][i]);
        }}
    }
    
    Keys[0] = StartKeys;
    Keys[1] = EndKeys;
    
    return Keys;
}

//int PathChecker(int a, std::vector<int> p){
//    for(int i = 0; i < p.size(); i++){
//        if(p[i] == a){
//            return 0;
//        }
//    }
//    return 1;
//}
//
//std::vector<std::vector<int>> AllPathsBFS(std::vector<std::vector<int>> Graph, std::vector<int> StartKeys, std::vector<int> EndKeys){
//
//    std::vector<std::vector<int>> CSet;
//    for(int i = 0; i < StartKeys.size(); i++){
//        for(int j = 0; j < EndKeys.size(); j++){
//
//            std::queue<std::vector<int>> pq;
//            std::vector<int> p;
//
//            p.push_back(StartKeys[i]);
//            pq.push(p);
//            while(!pq.empty()){
//                p = pq.front();
//                pq.pop();
//                int Curr = p[p.size() - 1];
//                if(Curr == EndKeys[j]){
//                    CSet.push_back(p);
//                }
//                for(int k = 0; k < Graph[Curr].size(); k++){
//                    if(PathChecker(Graph[Curr][k], p)){
//                        std::vector<int> pNew(p);
//                        pNew.push_back(Graph[Curr][k]);
//                        pq.push(pNew);
//                    }
//                }
//            }
//
//        }
//    }
//    return CSet;
//}

std::vector<std::vector<int>> AllPathsDFS(std::vector<std::vector<int>> Graph, std::vector<int> StartKeys, std::vector<int> EndKeys, std::vector<std::vector<int>> DKey, int lcslen){
    // Add a visitation rule and one vector that indicates whether there is a route to the target and another that includes every known route to the end.
    std::vector<std::vector<int>> CSet;
    std::vector<int> p(lcslen);
    //std::cout << "The number of nodes visited is: ";
    std::vector<bool> visited(Graph.size(),false);
    int NodeInc = 0;
    for(int i = 0; i < StartKeys.size(); i++){
            
            std::stack<int> s;
            
            s.push(StartKeys[i]);
        
            while(!s.empty()){
                //std::cout << "The stack is: " << s.size() << " elements long.\n";
                int Curr = s.top();
                s.pop();
                auto dInd = std::find(DKey[0].begin(), DKey[0].end(), Curr);
                
                int Depth = DKey[1][dInd - DKey[0].begin()];
                
                
                if(visited[Curr]){
                    //std::cout << "The most recent node revisited is: " <<Curr << "\n";
                   // Let's add the crazy shit
                    std::vector<std::vector<int>> VecSub;
                    for(int j = 0; j < CSet.size(); j++){
                        if(CSet[j][Depth] == Curr){
                            std::vector<int> Twig;
                            
                            for(int k = Depth; k < lcslen; k++){
                                Twig.push_back(CSet[j][k]);
                            }
                            VecSub.push_back(Twig);
                        }
                    }
//Switch to using std::unique here
                    
                    std::sort(VecSub.begin(),VecSub.end());
                    
                    VecSub.erase(std::unique(VecSub.begin(),VecSub.end()),VecSub.end());
                    
                    
                    for(int j = 0; j < VecSub.size(); j++){
                        for(int k = Depth; k < lcslen; k++){
                            p[k] = VecSub[j][(k - Depth)];
                        }
                        CSet.push_back(p);
                    }
                    
                } else {
                    NodeInc++;
                   // std::cout << NodeInc << " ";
                    //std::cout << "The most recent new node visited is: " <<Curr << "\n";
                    
                    visited[Curr] = true;
                    p[Depth] = Curr;
                
                    if(Depth == (lcslen-1)){
                        CSet.push_back(p);
                    
                    } else {
                
                for(int k = 0; k < Graph[Curr].size(); k++){
                    s.push(Graph[Curr][k]);
                }
                }
                }
            }
        
    }
    return CSet;
}

std::vector<std::vector<int>> AllPathsDFSNaive(std::vector<std::vector<int>> Graph, std::vector<int> StartKeys, std::vector<int> EndKeys, std::vector<std::vector<int>> DKey, int lcslen){
    // Add a visitation rule and one vector that indicates whether there is a route to the target and another that includes every known route to the end.
    std::vector<std::vector<int>> CSet;
    std::vector<int> p(lcslen);
    //std::cout << "The number of nodes visited is: ";
    //std::vector<bool> visited(Graph.size(),false);
    int NodeInc = 0;
    for(int i = 0; i < StartKeys.size(); i++){
            
            std::stack<int> s;
            
            s.push(StartKeys[i]);
        
            while(!s.empty()){
                //std::cout << "The stack is: " << s.size() << " elements long.\n";
                int Curr = s.top();
                s.pop();
                auto dInd = std::find(DKey[0].begin(), DKey[0].end(), Curr);
                
                int Depth = DKey[1][dInd - DKey[0].begin()];
                
                
               
                    NodeInc++;
                  //  std::cout << NodeInc << " ";
                    //std::cout << "The most recent new node visited is: " <<Curr << "\n";
                    
                    //visited[Curr] = true;
                    p[Depth] = Curr;
                
                    if(Depth == (lcslen-1)){
                        CSet.push_back(p);
                    
                    } else {
                
                for(int k = 0; k < Graph[Curr].size(); k++){
                    s.push(Graph[Curr][k]);
                }
                }
                }
            }
        
    
    return CSet;
}
std::vector<std::vector<int>> DnCSub(std::vector<std::vector<int>> Graph, std::vector<std::vector<int>> DKey, int MaxDepth, int MinDepth){
    // Add a visitation rule and one vector that indicates whether there is a route to the target and another that includes every known route to the end.
    std::vector<std::vector<int>> CSet;
    std::vector<std::vector<std::vector<int>>> EdgeTree(2);

    int F1;
    int F2;
    if(MaxDepth % 2 == 0){
        F1 = 0;
        F2 = 1;
    } else {
        F1 = 1;
        F2 = 0;
    }
    std::vector<int> LeafHolder(1);
    for(int i = 0; i < DKey[1].size(); i++){
        if(DKey[1][i] == MaxDepth){
            LeafHolder[0] = DKey[0][i];
            EdgeTree[F1].push_back(LeafHolder);
        }
    }
    for(int i = (MaxDepth - 1); i > (MinDepth - 1); i--){
        //std::cout << "The current depth being evaluated is: " << i + 1 << "\n";
        //Set flags to alternate objects
        if(i % 2 == 0){
            F1 = 0;
            F2 = 1;
        } else {
            F1 = 1;
            F2 = 0;
        }

        std::vector<int> DepKey;

        for(int j = 0; j < DKey[1].size(); j++){
            if(DKey[1][j] == i){
                DepKey.push_back(DKey[0][j]);
            }
        }
        EdgeTree[F1].clear();
        // Now we have a flagset & a set of nodes at this depth
        for(int j = 0; j < DepKey.size(); j++){
            int CurrNode = DepKey[j];
            std::vector<int> AdjVec = Graph[DepKey[j]];
            for(int k = 0; k < AdjVec.size(); k++){
                int CurrChild = AdjVec[k];
                // For each of these points, there will be one or more sets of "existing" paths
                std::vector<int> PathVec;
                for(int l = 0; l < EdgeTree[F2].size(); l++){
                    if(EdgeTree[F2][l][0] == CurrChild){
                        PathVec.push_back(l);
                    }
                }

                // Now we have a vector of keys as to which paths we can append.
                if(PathVec.size() > 0){
                    EdgeTree[F1].reserve(EdgeTree[F1].size() + PathVec.size());
                for(int l = 0; l < PathVec.size(); l++){
                    std::vector<int> Path(1, CurrNode);
                    Path.reserve(MaxDepth - i);
                    Path.insert(Path.begin()+1,EdgeTree[F2][l].begin(),EdgeTree[F2][l].end());
                    Path.shrink_to_fit();
                    EdgeTree[F1].push_back(Path);
                }} else {
                    std::vector<int> Path(1, CurrNode);
                    EdgeTree[F1].push_back(Path);
                }

            }
        }

    }
    // At the end, EdgeTree[F1] contains every path from root to leaf as a vector, so we need to pull only those of appropriate length
    int PathLength = MaxDepth - MinDepth + 1;
    
    for(int i = 0; i < EdgeTree[F1].size(); i++){
        if(EdgeTree[F1][i].size() == (PathLength)){
            CSet.push_back(EdgeTree[F1][i]);
        }
    }

    return CSet;
}

std::vector<std::vector<int>> AllPathsDnC(std::vector<std::vector<int>> Graph, std::vector<int> StartKeys, std::vector<int> EndKeys, std::vector<std::vector<int>> DKey, int lcslen){
    // Add a visitation rule and one vector that indicates whether there is a route to the target and another that includes every known route to the end.
    int PartDex = 5;
    std::vector<std::vector<int>> CSet;
    int MaxDepth = (lcslen - 1);
    int dodec = std::ceil((MaxDepth + 1) / PartDex);
    int end = dodec-1;
    //std::cout << dodec << " is the number of partitions to be included in the constructor set.\nThe current number of partitions evaluated is: ";
    
    std::vector<std::vector<std::vector<int>>> PathComp(dodec);
    for(int i = 0; i < end; i++){
        std::vector<std::vector<int>> Parti;
        int lB = i*PartDex;
        int uB = (i+1)*PartDex;
        
        Parti = DnCSub(Graph, DKey, uB, lB);
       // std::cout << i + 1 << " ";
        
        
        PathComp[i] = Parti;
    }
    int lB = end*PartDex;
    int uB = MaxDepth;
    
    PathComp[end] = DnCSub(Graph, DKey, uB, lB);
    //std::cout << dodec << "\n";
    std::vector<std::vector<int>> Ends(dodec);
    std::vector<std::vector<int>> Starts(dodec);
//    for(int i = 0; i < PathComp[0].size(); i++){
//        CSet.push_back(PathComp[0][i]);
//        Starts[0][i] = CSet[i][0];
//        Ends[0][i] = CSet[i][19];
//    }
    
    for(int i = 0; i < end; i++){
        for(int j = 0; j < PathComp[i].size(); j++){
            Starts[i].push_back(PathComp[i][j][0]);
            Ends[i].push_back(PathComp[i][j][PartDex]);
        }
    }
    
    for(int j = 0; j < PathComp[end].size(); j++){
        Starts[end].push_back(PathComp[end][j][0]);
        Ends[end].push_back(PathComp[end][j][uB - lB]);
    }
    
    std::vector<std::vector<std::vector<int>>> PathEdges(end);
    for(int i = 0; i < end; i++){
        std::vector<std::vector<int>> EdgeKeys;
        for(int j = 0; j < Ends[i].size(); j++){
            std::vector<int> EdgeVec;
            int EndPoint = Ends[i][j];
            std::vector<int> StartSet = Starts[i+1];
            for(int k = 0; k < Starts[i + 1].size(); k++){
                if(StartSet[k] == EndPoint){
                    EdgeVec.push_back(k);
                }
            }
            EdgeKeys.push_back(EdgeVec);
        }
        PathEdges[i] = EdgeKeys;
    }
    
//    for(int i = 0; i < PathComp[0].size(); i++){
//        CSet.push_back(PathComp[0][i]);
//        Starts[0][i] = CSet[i][0];
//        Ends[0][i] = CSet[i][19];
//    }
    // Okay, how do I keep this from being completely goddamn useless?
    std::vector<std::vector<std::vector<std::vector<int>>>> DdexPaths(2);
    int F1;
    int F2;
//    int incr = 0;
    for(int i = 0; i < PathComp[0].size(); i++){
        std::vector<std::vector<int>> Plug = {PathComp[0][i]};
        DdexPaths[0].push_back(Plug);
    }
    
    //std::cout << "The current number of partitions in the constructor set is: 1 ";
    
    for(int i = 1; i < dodec; i++){
        if(i % 2 == 0){
            F1 = 0;
            F2 = 1;
        } else {
            F1 = 1;
            F2 = 0;
        }
        int Pathdex = i-1;
        std::vector<int> EndSet = Ends[Pathdex];
        std::vector<int> StartSet = Starts[i];
        std::vector<std::vector<std::vector<int>>> DexSubPaths;
        for(int j = 0; j < EndSet.size(); j++){
            std::vector<std::vector<int>> PathSet;
            //std::vector<int> BasePath = DdexPaths[F2][j][0];
            int End = EndSet[j];
            // Do some work here
            for(int k = 0; k < StartSet.size(); k++){
                if(StartSet[k] == End){
                    std::vector<std::vector<int>> PathSub = DdexPaths[F2][j];
                    std::vector<int> AddPath = PathComp[i][k];
                    for(int l = 0; l < PathSub.size(); l++){
                        std::vector<int> BasePath = PathSub[l];
                        BasePath.insert(BasePath.end(),AddPath.begin() + 1,AddPath.end());
                        PathSet.push_back(BasePath);
                    }
                }
                
                DexSubPaths.push_back(PathSet);
            }
        }
        DdexPaths[F1] = DexSubPaths;
        //std::cout << i + 1 << " ";
    }
    // At the end of the loop, F1 is the one we want to unravel into each constructor
    for(int i = 0; i < DdexPaths[F1].size(); i++){
        for(int j = 0; j < DdexPaths[F1][i].size(); j++){
            CSet.push_back(DdexPaths[F1][i][j]);
        }
    }
   // std::cout << "\n";
    return CSet;
}
//
std::vector<std::vector<int>> AllPathsInvert(std::vector<std::vector<int>> Graph, std::vector<int> StartKeys, std::vector<int> EndKeys, std::vector<std::vector<int>> DKey, int lcslen){
    // Add a visitation rule and one vector that indicates whether there is a route to the target and another that includes every known route to the end.
    std::vector<std::vector<int>> CSet;
    std::vector<std::vector<std::vector<int>>> EdgeTree(2);

    int MaxDepth = *std::max_element(DKey[1].begin(),DKey[1].end());
    int F1;
    int F2;
    if(MaxDepth % 2 == 0){
        F1 = 0;
        F2 = 1;
    } else {
        F1 = 1;
        F2 = 0;
    }
    std::vector<int> LeafHolder(1);
    for(int i = 0; i < DKey[1].size(); i++){
        if(DKey[1][i] == MaxDepth){
            LeafHolder[0] = DKey[0][i];
            EdgeTree[F1].push_back(LeafHolder);
        }
    }
    for(int i = (MaxDepth - 1); i > -1; i--){
        //std::cout << "The current depth being evaluated is: " << i + 1 << "\n";
        //Set flags to alternate objects
        if(i % 2 == 0){
            F1 = 0;
            F2 = 1;
        } else {
            F1 = 1;
            F2 = 0;
        }

        std::vector<int> DepKey;

        for(int j = 0; j < DKey[1].size(); j++){
            if(DKey[1][j] == i){
                DepKey.push_back(DKey[0][j]);
            }
        }
        EdgeTree[F1].clear();
        // Now we have a flagset & a set of nodes at this depth
        for(int j = 0; j < DepKey.size(); j++){
            int CurrNode = DepKey[j];
            std::vector<int> AdjVec = Graph[DepKey[j]];
            for(int k = 0; k < AdjVec.size(); k++){
                int CurrChild = AdjVec[k];
                // For each of these points, there will be one or more sets of "existing" paths
                std::vector<int> PathVec;
                for(int l = 0; l < EdgeTree[F2].size(); l++){
                    if(EdgeTree[F2][l][0] == CurrChild){
                        PathVec.push_back(l);
                    }
                }

                // Now we have a vector of keys as to which paths we can append.
                if(PathVec.size() > 0){
                    EdgeTree[F1].reserve(EdgeTree[F1].size() + PathVec.size());
                for(int l = 0; l < PathVec.size(); l++){
                    std::vector<int> Path(1, CurrNode);
                    Path.reserve(MaxDepth - i);
                    Path.insert(Path.begin()+1,EdgeTree[F2][l].begin(),EdgeTree[F2][l].end());
                    Path.shrink_to_fit();
                    EdgeTree[F1].push_back(Path);
                }} else {
                    std::vector<int> Path(1, CurrNode);
                    EdgeTree[F1].push_back(Path);
                }

            }
        }

    }
    // At the end, EdgeTree[F1] contains every path from root to leaf as a vector, so we need to pull only those of length == MaxDepth
    for(int i = 0; i < EdgeTree[F1].size(); i++){
        if(EdgeTree[F1][i].size() == (MaxDepth+1)){
            CSet.push_back(EdgeTree[F1][i]);
        }
    }

    return CSet;
}


std::vector<std::vector<int>> NodeLocConvert(std::vector<std::vector<int>> & CSet_, std::vector<std::vector<int>> & DKey){
    std::vector<std::vector<int>> CSet(CSet_);
    std::vector<double> NodeVec(DKey[0].size(), NAN);
    for(int i = 0; i < CSet_.size(); i++){
        
        for(int j = 0; j < CSet_[i].size(); j++){
            int Node = CSet_[i][j];
            if(NodeVec[Node] > -1){
                
                CSet[i][j] = int(NodeVec[Node]);
                
            } else {
                
                auto NInd = find(DKey[0].begin(), DKey[0].end(), Node);
                int NodeC = DKey[2][NInd - DKey[0].begin()];
                NodeVec[Node] = double(NodeC);
                CSet[i][j] = NodeC;
                
            }
        }
    }
    
    return CSet;
}

void LCSConstructHunt(std::vector<std::vector<int>> LCSset, std::vector<int> Fish1Seq, std::vector<int> Fish2Seq, std::vector<std::tuple<std::vector<std::string>,std::vector<std::vector<double>>,std::vector<std::vector<double>>,double>> & PullSet, std::vector<double> TSeqArr1, std::vector<double> TSeqDep1, std::vector<double> TSeqArr2, std::vector<double> TSeqDep2, std::vector<std::string> Method, std::vector<std::string> LocLib, int lcslen, int Size1, int Size2){
    std::vector<std::vector<std::tuple<std::vector<std::string>,std::vector<std::vector<double>>,std::vector<std::vector<double>>,double>>> Candidates(Method.size());
    
    int l = Method.size();
    
    int BestConsts[l][2];
    
    
    
    
    
    
    std::vector<std::vector<double>> DistLUT (Size1, std::vector<double> (Size2, NAN));
    std::vector<std::vector<double>> DiffLUT (Size1, std::vector<double> (Size2, NAN));
    std::vector<std::vector<std::vector<std::vector<double>>>> StepLUT (Size1, std::vector<std::vector<std::vector<double>>>(Size1, std::vector<std::vector<double>>(Size2,std::vector<double>(Size2, NAN))));
    
    for(int i = 0; i<LCSset.size(); i++){
        std::vector<std::vector<int>> FMT1(lcslen);
        std::vector<std::vector<int>> FMT2(lcslen);
        for(int j = 0; j<lcslen; j++){
            std::vector<int> Const1;
            std::vector<int> Const2;
            for(int k = 0; k<Fish1Seq.size(); k++){
                if(Fish1Seq[k] == LCSset[i][j]){
                    Const1.push_back(k);
                }}
                
                for(int k = 0; k<Fish2Seq.size(); k++){
                    if(Fish2Seq[k] == LCSset[i][j]){
                        Const2.push_back(k);
                    }}
                    
                    FMT1[j] = Const1;
                    FMT2[j] = Const2;
            }
        std::vector<int> Const1_(lcslen);
        std::vector<int> Const2_(lcslen);
        std::vector<std::vector<int>> CSet1;
        std::vector<std::vector<int>> CSet2;
        // Remember, this is still in a for loop to step through each LCS! Stepping through each constructor is done by the Construct Stepper
        std::vector<std::vector<int>> DKey1(3);
        std::vector<int> N;
        std::vector<int> D;
        std::vector<int> C;
        std::vector<std::vector<int>> CKey1(FMT1);
        int Gind = 0;
        for(int j = 0; j < FMT1.size(); j++){
            for(int k = 0; k < FMT1[j].size(); k++){
                CKey1[j][k] = Gind;
                N.push_back(Gind);
                D.push_back(j);
                C.push_back(FMT1[j][k]);
                Gind++;
                }
        }
        DKey1[0] = N;
        DKey1[1] = D;
        DKey1[2] = C;
        
        std::vector<std::vector<int>> DKey2(3);
        N.clear();
        D.clear();
        C.clear();
        std::vector<std::vector<int>> CKey2(FMT2);
        Gind = 0;
        for(int j = 0; j < FMT2.size(); j++){
            for(int k = 0; k < FMT2[j].size(); k++){
                CKey2[j][k] = Gind;
                N.push_back(Gind);
                D.push_back(j);
                C.push_back(FMT2[j][k]);
                Gind++;
                }
        }
        DKey2[0] = N;
        DKey2[1] = D;
        DKey2[2] = C;
        
        std::vector<std::vector<int>> CGraph1;
        std::vector<std::vector<int>> CGraph2;

        int jLim = (FMT1.size()-1);
        for(int j = 0; j<(FMT1.size()); j++){
            for(int k = 0; k<FMT1[j].size(); k++){
                
                int CPar = FMT1[j][k];
                
                std::vector<int> CAdj;
                if(jLim != j){
                
                for(int m = 0; m<FMT1[j+1].size(); m++){
                    int CChil = FMT1[j+1][m];
                    if(CPar < CChil){
                        CAdj.push_back(CKey1[j+1][m]);
                    }
                }}
                CGraph1.push_back(CAdj);
        
            }
        }

        
        jLim = (FMT2.size()-1);
        
        for(int j = 0; j<(FMT2.size()); j++){
            for(int k = 0; k<FMT2[j].size(); k++){
                
                int CPar = FMT2[j][k];
                
                std::vector<int> CAdj;
                if(jLim != j){
                for(int m = 0; m< FMT2[j+1].size(); m++){
                    int CChil = FMT2[j+1][m];
                    
                    if(CPar < CChil){
                        CAdj.push_back(CKey2[j+1][m]);
                    }
                    
                }}
                CGraph2.push_back(CAdj);
            }
        }
        // Whoops need to translate the adjacency to the position in the vector.
        // Now we need to add a way to give node "keys" for start and end points
        
        std::vector<std::vector<int>> Keys1 = KeyFinder(DKey1);
        std::vector<std::vector<int>> Keys2 = KeyFinder(DKey2);
        
        
        std::vector<std::vector<int>> UnTLCSet1;
        std::vector<std::vector<int>> UnTLCSet2;
        // Now for an iterative search approach to see how much we pull out!
       // std::vector<std::vector<int>> UnTLCSet1 = AllPathsInvert(CGraph1, Keys1[0], Keys1[1], DKey1, lcslen);
       // std::vector<std::vector<int>> UnTLCSet2 = AllPathsInvert(CGraph2, Keys2[0], Keys2[1], DKey2, lcslen);
//        if(lcslen < 40){
//             UnTLCSet1 = AllPathsInvert(CGraph1, Keys1[0], Keys1[1], DKey1, lcslen);
//             UnTLCSet2 = AllPathsInvert(CGraph2, Keys2[0], Keys2[1], DKey2, lcslen);
//        } else {
//            UnTLCSet1 = AllPathsDnC(CGraph1, Keys1[0], Keys1[1], DKey1, lcslen);
//            UnTLCSet2 = AllPathsDnC(CGraph2, Keys2[0], Keys2[1], DKey2, lcslen);
//        }
//
        
        UnTLCSet1 = AllPathsDFSNaive(CGraph1, Keys1[0], Keys1[1], DKey1, lcslen);
        UnTLCSet2 = AllPathsDFSNaive(CGraph2, Keys2[0], Keys2[1], DKey2, lcslen);
//        std::vector<std::vector<int>> UnTLCSet1 = AllPathsBFS(CGraph1, Keys1[0], Keys1[1]);
//        std::vector<std::vector<int>> UnTLCSet2 = AllPathsBFS(CGraph2, Keys2[0], Keys2[1]);

        //NodeLocConvert is helllla bugged.
        // Currently has issues with some cases of LCS... maybe there's an upstream issue?
        // Adjacency table is fine... Check traversal?
        // Oh kill me and turn the corpse into proper fertilizer. It's in the DFS alg somewhere.
        CSet1 = NodeLocConvert(UnTLCSet1, DKey1);
        CSet2 = NodeLocConvert(UnTLCSet2, DKey2);
        
        
        
        //LCSConstructStepper(LCSset[i], FMT1, CSet1, Const1_, 0, 0, lcslen);
        //LCSConstructStepper(LCSset[i], FMT2, CSet2, Const2_, 0, 0, lcslen);
        // At this point, our CSets are built, our TSeq's are read in, we are as ready as we can be.
        std::tuple<std::vector<std::string>,std::vector<std::vector<double>>,std::vector<std::vector<double>>,std::vector<double>> LCSConst;



        
        // Construct selector will be replaced with a "raw" version that accounts for any included methods.
        // We need to find a way to either evaluate as we go from this table or SOMETHING to make this faster.
        // Put the logic switches in here
        std::vector<double> BestDistVec;
        std::vector<double> BestPhaseVec;
        std::vector<double> BestStepVec;
        double BestDist = NAN;
        double BestPhase = NAN;
        double BestStep = NAN;
         
        std::vector<std::vector<std::vector<std::vector<double>>>> BestConst(l);
        std::vector<std::vector<double>> BestC1;
        std::vector<std::vector<double>> BestC2;

       std::vector<std::vector<double>> tVec1;
       std::vector<std::vector<double>> tVec2;
       // Here is where the split into unknown territory begins
        if(CSet1.size() == 1 && CSet2.size() == 1){
            std::vector<std::vector<std::vector<double>>> ConstrTable1(1);
            std::vector<std::vector<std::vector<double>>> ConstrTable2(1);
            
                     std::vector<std::vector<double>> TPairVec1(lcslen, std::vector<double>(2, NAN));
                     
                     for(int j = 0; j<CSet1[0].size(); j++){
                         std::vector<double> TPair1(2, NAN);
                         TPair1[0] = (TSeqArr1[CSet1[0][j]]);
                         TPair1[1] = (TSeqDep1[CSet1[0][j]]);
                         TPairVec1[j] = (TPair1);
                     }
                     ConstrTable1[0] = (TPairVec1);
                 
                 
                    std::vector<std::vector<double>> TPairVec2(lcslen, std::vector<double>(2, NAN));
                     
                     for(int j = 0; j<CSet2[0].size(); j++){
                         std::vector<double> TPair2(2, NAN);
                         TPair2[0] = (TSeqArr2[CSet2[0][j]]);
                         TPair2[1] = (TSeqDep2[CSet2[0][j]]);
                         TPairVec2[j] = (TPair2);
                     }
                     ConstrTable2[0] = (TPairVec2);
                 
            for(int k = 0; k < l; k++){
            // Assemble the output and end this
            BestC1.insert(BestC1.end(),ConstrTable1[0].begin(),ConstrTable1[0].end());
            BestC2.insert(BestC2.end(),ConstrTable2[0].begin(),ConstrTable2[0].end());
            BestConst[k].push_back(BestC1);
            BestConst[k].push_back(BestC2);
            }
                
                for(int i = 0; i<ConstrTable1.size(); i++){
                    tVec1 = ConstrTable1[i];
                    
                    for(int j = 0; j<ConstrTable2.size(); j++){
                        std::vector<double> DistVec(lcslen, NAN);
                        std::vector<double> PhaseVec(lcslen, NAN);
                        std::vector<double> StepVec(lcslen, NAN);
                        tVec2 = ConstrTable2[j];
                        
                        std::vector<double> DiffVec(lcslen, NAN);
                        double DiffMean;
                        for(int k = 0; k<ConstrTable1[0].size(); k++){
                            if(!(DiffLUT[CSet1[0][k]][CSet2[0][k]] > 0)){
                            DiffVec[k] = ((tVec1[k][0] - tVec2[k][0]) + (tVec1[k][1] - tVec2[k][1]));
                                DiffLUT[CSet1[0][k]][CSet2[0][k]] = DiffVec[k];
                            } else {
                                DiffVec[k] = DiffLUT[CSet1[0][k]][CSet2[0][k]];
                            }
                        }
                        
                        double DiffSum = std::accumulate(DiffVec.begin(), DiffVec.end(), 0LL);
                        DiffMean = DiffSum / (2 * DiffVec.size());
                        
                        if(!(DistLUT[CSet1[0][0]][CSet2[0][0]] > 0)){
                        DistVec[0] = ((std::pow((tVec1[0][0] - tVec2[0][0]), 2) /2) + (std::pow((tVec1[0][1]-tVec2[0][1]),2) /2));
                            DistLUT[CSet1[0][0]][CSet2[0][0]] = DistVec[0];
                        } else{
                            DistVec[0] = DistLUT[CSet1[0][0]][CSet2[0][0]];
                        }
                        
                        PhaseVec[0] = (std::pow((tVec1[0][0]-tVec2[0][0] - DiffMean), 2)  +  std::pow((tVec1[0][1]-tVec2[0][1] - DiffMean), 2));

                        StepVec[0] = (0);
                        
                        
                        
                        for(int k = 1; k<lcslen; k++){
                            
                            if(!(DistLUT[CSet1[0][k]][CSet2[0][k]] > 0)){
                            DistVec[k] = ((std::pow((tVec1[k][0] - tVec2[k][0]), 2) /2) + (std::pow((tVec1[k][1]-tVec2[k][1]),2) /2));
                                DistLUT[CSet1[0][k]][CSet2[0][k]] = DistVec[k];
                            } else {
                                DistVec[k] = DistLUT[CSet1[0][k]][CSet2[0][k]];
                            }
                            
                            PhaseVec[k] = (std::pow((tVec1[k][0]-tVec2[k][0] - DiffMean), 2)  +  std::pow((tVec1[k][1]-tVec2[k][1] - DiffMean), 2));
                            
                            if(!(StepLUT[CSet1[0][k-1]][CSet1[0][k]][CSet2[0][k-1]][CSet2[0][k]] > 0)){
                            StepVec[k] = ((std::pow((tVec1[k][0]-tVec1[k-1][1]) - (tVec2[k][0]-tVec2[k-1][1]),2)/2)  + (std::pow((tVec1[k][1]-tVec1[k][0]) - (tVec2[k][1]-tVec2[k][0]), 2)/2));
                                StepLUT[CSet1[0][k-1]][CSet1[0][k]][CSet2[0][k-1]][CSet2[0][k]] = StepVec[k];
                            } else {
                                StepVec[k] = StepLUT[CSet1[0][k-1]][CSet1[0][k]][CSet2[0][k-1]][CSet2[0][k]];
                            }
                            
                        }
                        
                        BestDist = std::accumulate(DistVec.begin(), DistVec.end(), 0LL);

                        BestPhase = std::accumulate(PhaseVec.begin(), PhaseVec.end(), 0LL);

                        BestStep = std::accumulate(StepVec.begin(), StepVec.end(), 0LL);

                    }
                }
        } else {
/*
 
    
         for(int i = 0; i<CSet1.size(); i++){
                 for(int k = 0; i<CSet2.size(); i++){

                 std::vector<std::vector<double>> TPairVec1;

                 std::vector<std::vector<double>> TPairVec2;
 std::vector<double> DistVec;
 std::vector<double> PhaseVec;
 std::vector<double> StepVec;
                         for(int j = 0; j<CSet1[0].size(); j++){
                             std::vector<double> TPair1;
                             TPair1.push_back(TSeqArr1[CSet1[i][j]]);
                             TPair1.push_back(TSeqDep1[CSet1[i][j]]);
                             TPairVec1.push_back(TPair1);
                             std::vector<double> TPair2;
                             TPair2.push_back(TSeqArr2[CSet2[k][j]]);
                             TPair2.push_back(TSeqDep2[CSet2[k][j]]);
                             TPairVec2.push_back(TPair2);
                         }
                 // Evaluation and storage terms here.
             
         }}
         

 */
                    // METHODS DIFFERENTIATION SECTION
                     

                     if(std::find(Method.begin(), Method.end(), "LeastDist") != Method.end()){
                         if(std::find(Method.begin(), Method.end(), "PhaSim") != Method.end()){
                             if(std::find(Method.begin(), Method.end(), "LeastStep") != Method.end()){
                                 // All three goes here
                                 BestDist = NAN;
                                 BestPhase = NAN;
                                 BestStep = NAN;
                                 
                                 for(int i = 0; i<CSet1.size(); i++){
                                         for(int j = 0; j<CSet2.size(); j++){
                                             
                                         std::vector<std::vector<double>> tVec1(lcslen,std::vector<double> (2, NAN));

                                         std::vector<std::vector<double>> tVec2(lcslen,std::vector<double> (2, NAN));

                                             std::vector<double> DistVec(lcslen, NAN);
                                             std::vector<double> PhaseVec(lcslen, NAN);
                                             std::vector<double> StepVec(lcslen, NAN);
                                                 for(int m = 0; m<lcslen; m++){
                                                     std::vector<double> TPair1(2, NAN);
                                                     TPair1[0] = TSeqArr1[CSet1[i][m]];
                                                     TPair1[1] = TSeqDep1[CSet1[i][m]];
                                                     tVec1[m] = TPair1;
                                                     std::vector<double> TPair2(2, NAN);
                                                     TPair2[0] = TSeqArr2[CSet2[j][m]];
                                                     TPair2[1] = TSeqDep2[CSet2[j][m]];
                                                     tVec2[m] = TPair2;
                                                 }
                                             
                                             std::vector<double> DiffVec(lcslen, NAN);
                                             double DiffMean;
                                             for(int k = 0; k<lcslen; k++){
                                                 if(!(DiffLUT[CSet1[i][k]][CSet2[j][k]] > 0)){
                                                 DiffVec[k] = ((tVec1[k][0] - tVec2[k][0]) + (tVec1[k][1] - tVec2[k][1]));
                                                     DiffLUT[CSet1[i][k]][CSet2[j][k]] = DiffVec[k];
                                                 } else {
                                                     DiffVec[k] = DiffLUT[CSet1[i][k]][CSet2[j][k]];
                                                 }
                                             }
                                             
                                             double DiffSum = std::accumulate(DiffVec.begin(), DiffVec.end(), 0LL);
                                             DiffMean = DiffSum / (2 * DiffVec.size());
                                             
                                             if(!(DistLUT[CSet1[i][0]][CSet2[j][0]] > 0)){
                                             DistVec[0] = ((std::pow((tVec1[0][0] - tVec2[0][0]), 2) /2) + (std::pow((tVec1[0][1]-tVec2[0][1]),2) /2));
                                                 DistLUT[CSet1[i][0]][CSet2[j][0]] = DistVec[0];
                                             } else{
                                                 DistVec[0] = DistLUT[CSet1[i][0]][CSet2[j][0]];
                                             }
                                             
                                             PhaseVec[0] = (std::pow((tVec1[0][0]-tVec2[0][0] - DiffMean), 2)  +  std::pow((tVec1[0][1]-tVec2[0][1] - DiffMean), 2));
                                             
                                             StepVec[0] = (0);
                                             
                                         for(int k = 1; k<lcslen; k++){
                                             
                                             if(!(DistLUT[CSet1[i][k]][CSet2[j][k]] > 0)){
                                             DistVec[k] = ((std::pow((tVec1[k][0] - tVec2[k][0]), 2) /2) + (std::pow((tVec1[k][1]-tVec2[k][1]),2) /2));
                                                 DistLUT[CSet1[i][k]][CSet2[j][k]] = DistVec[k];
                                             } else {
                                                 DistVec[k] = DistLUT[CSet1[i][k]][CSet2[j][k]];
                                             }
                                             
                                             PhaseVec[k] = (std::pow((tVec1[k][0]-tVec2[k][0] - DiffMean), 2)  +  std::pow((tVec1[k][1]-tVec2[k][1] - DiffMean), 2));
                                             
                                             if(!(StepLUT[CSet1[i][k-1]][CSet1[i][k]][CSet2[j][k-1]][CSet2[j][k]] > 0)){
                                             StepVec[k] = ((std::pow((tVec1[k][0]-tVec1[k-1][1]) - (tVec2[k][0]-tVec2[k-1][1]),2)/2)  + (std::pow((tVec1[k][1]-tVec1[k][0]) - (tVec2[k][1]-tVec2[k][0]), 2)/2));
                                                 StepLUT[CSet1[i][k-1]][CSet1[i][k]][CSet2[j][k-1]][CSet2[j][k]] = StepVec[k];
                                             } else {
                                                 StepVec[k] = StepLUT[CSet1[i][k-1]][CSet1[i][k]][CSet2[j][k-1]][CSet2[j][k]];
                                             }
                                         }
                                         double CurrDist = std::accumulate(DistVec.begin(),DistVec.end(),0LL);
                                         double CurrPhase = std::accumulate(PhaseVec.begin(),PhaseVec.end(),0LL);
                                         double CurrStep = std::accumulate(StepVec.begin(),StepVec.end(),0LL);
                                         if(!(BestDist < CurrDist)){
                                             BestDist = CurrDist;
                                             BestConsts[0][0] = i;
                                             BestConsts[0][1] = j;
//                                             BestDistVec = DistVec;
                                         }
                                         if(!(BestPhase < CurrPhase)){
                                             BestPhase = CurrPhase;
                                             BestConsts[1][0] = i;
                                             BestConsts[1][1] = j;
//                                             BestPhaseVec = PhaseVec;
                                         }
                                         if(!(BestStep < CurrStep)){
                                             BestStep = CurrStep;
                                             BestConsts[2][0] = i;
                                             BestConsts[2][1] = j;
//                                             BestStepVec = StepVec;
                                         }
                                     }
                                 }
                             } else {
                                 // LeastDist and PhaSim goes here
                                 BestDist = NAN;
                                 BestPhase = NAN;
                                 for(int i = 0; i<CSet1.size(); i++){
                                         for(int j = 0; j<CSet2.size(); j++){

                                             std::vector<std::vector<double>> tVec1(lcslen,std::vector<double> (2, NAN));

                                             std::vector<std::vector<double>> tVec2(lcslen,std::vector<double> (2, NAN));

                                                 std::vector<double> DistVec(lcslen, NAN);
                                                 std::vector<double> PhaseVec(lcslen, NAN);

                                                     for(int m = 0; m<lcslen; m++){
                                                         std::vector<double> TPair1(2, NAN);
                                                         TPair1[0] = TSeqArr1[CSet1[i][m]];
                                                         TPair1[1] = TSeqDep1[CSet1[i][m]];
                                                         tVec1[m] = TPair1;
                                                         std::vector<double> TPair2(2, NAN);
                                                         TPair2[0] = TSeqArr2[CSet2[j][m]];
                                                         TPair2[1] = TSeqDep2[CSet2[j][m]];
                                                         tVec2[m] = TPair2;
                                                     }
                                                 
                                                 std::vector<double> DiffVec(lcslen, NAN);
                                             double DiffMean;
                                             for(int k = 0; k<lcslen; k++){
                                                    if(!(DiffLUT[CSet1[0][k]][CSet2[0][k]] > 0)){
                                                        DiffVec[k] = ((tVec1[k][0] - tVec2[k][0]) + (tVec1[k][1] - tVec2[k][1]));
                                                        DiffLUT[CSet1[0][k]][CSet2[0][k]] = DiffVec[k];
                                                    } else {
                                                        DiffVec[k] = DiffLUT[CSet1[0][k]][CSet2[0][k]];
                                                    }
                                             }
                                             
                                             double DiffSum = std::accumulate(DiffVec.begin(), DiffVec.end(), 0LL);
                                             DiffMean = DiffSum / (2 * DiffVec.size());
                                             
                                             
                                         for(int k = 0; k<lcslen; k++){
                                             if(!(DistLUT[CSet1[i][k]][CSet2[j][k]] > 0)){
                                             DistVec[k] = ((std::pow((tVec1[k][0] - tVec2[k][0]), 2) /2) + (std::pow((tVec1[k][1]-tVec2[k][1]),2) /2));
                                                 DistLUT[CSet1[i][k]][CSet2[j][k]] = DistVec[k];
                                             } else {
                                                 DistVec[k] = DistLUT[CSet1[i][k]][CSet2[j][k]];
                                             }
                                             
                                             PhaseVec[k] = (std::pow(tVec1[k][0]-tVec2[k][0] - DiffMean, 2)  +  std::pow(tVec1[k][1]-tVec2[k][1] - DiffMean, 2));
                                         }
                                         double CurrDist = std::accumulate(DistVec.begin(),DistVec.end(),0LL);
                                         double CurrPhase = std::accumulate(PhaseVec.begin(),PhaseVec.end(),0LL);
                                         if(!(BestDist < CurrDist)){
                                             BestDist = CurrDist;
                                             BestConsts[0][0] = i;
                                             BestConsts[0][1] = j;
//                                             BestDistVec = DistVec;
                                         }
                                         if(!(BestPhase < CurrPhase)){
                                             BestPhase = CurrPhase;
                                             BestConsts[1][0] = i;
                                             BestConsts[1][1] = j;
//                                             BestPhaseVec = PhaseVec;
                                         }
                                     }
                                 }
                             }
                         } else if(std::find(Method.begin(), Method.end(), "LeastStep") != Method.end()){
                             // LeastDist and LeastStep goes here
                            BestDist = NAN;
                            BestStep = NAN;
                             
                             for(int i = 0; i<CSet1.size(); i++){
                                     for(int j = 0; j<CSet2.size(); j++){

                                         std::vector<std::vector<double>> tVec1(lcslen,std::vector<double> (2, NAN));

                                         std::vector<std::vector<double>> tVec2(lcslen,std::vector<double> (2, NAN));

                                             std::vector<double> DistVec(lcslen, NAN);
                                             std::vector<double> StepVec(lcslen, NAN);
                                                 for(int m = 0; m<lcslen; m++){
                                                     std::vector<double> TPair1(2, NAN);
                                                     TPair1[0] = TSeqArr1[CSet1[i][m]];
                                                     TPair1[1] = TSeqDep1[CSet1[i][m]];
                                                     tVec1[m] = TPair1;
                                                     std::vector<double> TPair2(2, NAN);
                                                     TPair2[0] = TSeqArr2[CSet2[j][m]];
                                                     TPair2[1] = TSeqDep2[CSet2[j][m]];
                                                     tVec2[m] = TPair2;
                                                 }
                                             
                                         
                                         if(!(DistLUT[CSet1[0][0]][CSet2[0][0]] > 0)){
                                         DistVec[0] = ((std::pow((tVec1[0][0] - tVec2[0][0]), 2) /2) + (std::pow((tVec1[0][1]-tVec2[0][1]),2) /2));
                                             DistLUT[CSet1[0][0]][CSet2[0][0]] = DistVec[0];
                                         } else{
                                             DistVec[0] = DistLUT[CSet1[0][0]][CSet2[0][0]];
                                         }
                                                                                  
                                         StepVec[0] = (0);
                                         
                                     for(int k = 1; k<lcslen; k++){
                                         if(!(DistLUT[CSet1[i][k]][CSet2[j][k]] > 0)){
                                             DistVec[k] = ((std::pow((tVec1[k][0] - tVec2[k][0]), 2) /2) + (std::pow((tVec1[k][1]-tVec2[k][1]),2) /2));
                                                 DistLUT[CSet1[i][k]][CSet2[j][k]] = DistVec[k];
                                             } else {
                                                 DistVec[k] = DistLUT[CSet1[i][k]][CSet2[j][k]];
                                             }
                                         
                                         if(!(StepLUT[CSet1[i][k-1]][CSet1[i][k]][CSet2[j][k-1]][CSet2[j][k]] > 0)){
                                         StepVec[k] = ((std::pow((tVec1[k][0]-tVec1[k-1][1]) - (tVec2[k][0]-tVec2[k-1][1]),2)/2)  + (std::pow((tVec1[k][1]-tVec1[k][0]) - (tVec2[k][1]-tVec2[k][0]), 2)/2));
                                             StepLUT[CSet1[i][k-1]][CSet1[i][k]][CSet2[j][k-1]][CSet2[j][k]] = StepVec[k];
                                         } else {
                                             StepVec[k] = StepLUT[CSet1[i][k-1]][CSet1[i][k]][CSet2[j][k-1]][CSet2[j][k]];
                                         }
                                     }
                                     double CurrDist = std::accumulate(DistVec.begin(),DistVec.end(),0LL);
                                     double CurrStep = std::accumulate(StepVec.begin(),StepVec.end(),0LL);
                                     
                                     if(!(BestDist < CurrDist)){
                                         BestDist = CurrDist;
                                         BestConsts[0][0] = i;
                                         BestConsts[0][1] = j;
//                                         BestDistVec = DistVec;
                                     }

                                     if(!(BestStep < CurrStep)){
                                         BestStep = CurrStep;
                                         BestConsts[1][0] = i;
                                         BestConsts[1][1] = j;
//                                         BestStepVec = StepVec;
                                     }
                                 }
                             }
                         } else {
                      // Just LeastDist goes here
                      BestDist = NAN;
                             for(int i = 0; i<CSet1.size(); i++){
                                     for(int j = 0; j<CSet2.size(); j++){

                                         std::vector<std::vector<double>> tVec1(lcslen,std::vector<double> (2, NAN));

                                         std::vector<std::vector<double>> tVec2(lcslen,std::vector<double> (2, NAN));

                                             std::vector<double> DistVec(lcslen, NAN);

                                                 for(int m = 0; m<lcslen; m++){
                                                     std::vector<double> TPair1(2, NAN);
                                                     TPair1[0] = TSeqArr1[CSet1[i][m]];
                                                     TPair1[1] = TSeqDep1[CSet1[i][m]];
                                                     tVec1[m] = TPair1;
                                                     std::vector<double> TPair2(2, NAN);
                                                     TPair2[0] = TSeqArr2[CSet2[j][m]];
                                                     TPair2[1] = TSeqDep2[CSet2[j][m]];
                                                     tVec2[m] = TPair2;
                                                 }
                                             
                              for(int k = 0; k<lcslen; k++){
                                  if(!(DistLUT[CSet1[i][k]][CSet2[j][k]] > 0)){
                                             DistVec[k] = ((std::pow((tVec1[k][0] - tVec2[k][0]), 2) /2) + (std::pow((tVec1[k][1]-tVec2[k][1]),2) /2));
                                                 DistLUT[CSet1[i][k]][CSet2[j][k]] = DistVec[k];
                                             } else {
                                                 DistVec[k] = DistLUT[CSet1[i][k]][CSet2[j][k]];
                                             }
                              }
                              double CurrDist = std::accumulate(DistVec.begin(),DistVec.end(),0LL);
                              if(!(BestDist < CurrDist)){
                                  BestDist = CurrDist;
                                  BestConsts[0][0] = i;
                                  BestConsts[0][1] = j;
//                                  BestDistVec = DistVec;
                              }
                          }
                      }}
                         
                     } else if(std::find(Method.begin(), Method.end(), "PhaSim") != Method.end()){
                         if(std::find(Method.begin(), Method.end(), "LeastStep") != Method.end()){
                             // PhaSim and LeastStep goes here
                              BestPhase = NAN;
                              BestStep = NAN;
                             for(int i = 0; i<CSet1.size(); i++){
                                     for(int j = 0; j<CSet2.size(); j++){

                                         std::vector<std::vector<double>> tVec1(lcslen,std::vector<double> (2, NAN));

                                         std::vector<std::vector<double>> tVec2(lcslen,std::vector<double> (2, NAN));

                                             std::vector<double> PhaseVec(lcslen, NAN);
                                             std::vector<double> StepVec(lcslen, NAN);
                                                 for(int m = 0; m<lcslen; m++){
                                                     std::vector<double> TPair1(2, NAN);
                                                     TPair1[0] = TSeqArr1[CSet1[i][m]];
                                                     TPair1[1] = TSeqDep1[CSet1[i][m]];
                                                     tVec1[m] = TPair1;
                                                     std::vector<double> TPair2(2, NAN);
                                                     TPair2[0] = TSeqArr2[CSet2[j][m]];
                                                     TPair2[1] = TSeqDep2[CSet2[j][m]];
                                                     tVec2[m] = TPair2;
                                                 }
                                             
                                             std::vector<double> DiffVec(lcslen, NAN);
                                         double DiffMean;
                                         for(int k = 0; k<lcslen; k++){
                                                if(!(DiffLUT[CSet1[0][k]][CSet2[0][k]] > 0)){
                                                    DiffVec[k] = ((tVec1[k][0] - tVec2[k][0]) + (tVec1[k][1] - tVec2[k][1]));
                                                    DiffLUT[CSet1[0][k]][CSet2[0][k]] = DiffVec[k];
                                                } else {
                                                    DiffVec[k] = DiffLUT[CSet1[0][k]][CSet2[0][k]];
                                                }
                                         }
                                         
                                         double DiffSum = std::accumulate(DiffVec.begin(), DiffVec.end(), 0LL);
                                         DiffMean = DiffSum / (2 * DiffVec.size());
                                         
                                         PhaseVec[0] = (std::pow(tVec1[0][0]-tVec2[0][0] - DiffMean, 2)  +  std::pow(tVec1[0][1]-tVec2[0][1] - DiffMean, 2));
                                         
                                         StepVec[0] = (0);
                                         
                                     for(int k = 1; k<lcslen; k++){
                                         PhaseVec[k] = (std::pow(tVec1[k][0]-tVec2[k][0] - DiffMean, 2)  +  std::pow(tVec1[k][1]-tVec2[k][1] - DiffMean, 2));
                                         
                                         if(!(StepLUT[CSet1[i][k-1]][CSet1[i][k]][CSet2[j][k-1]][CSet2[j][k]] > 0)){
                                         StepVec[k] = ((std::pow((tVec1[k][0]-tVec1[k-1][1]) - (tVec2[k][0]-tVec2[k-1][1]),2)/2)  + (std::pow((tVec1[k][1]-tVec1[k][0]) - (tVec2[k][1]-tVec2[k][0]), 2)/2));
                                             StepLUT[CSet1[i][k-1]][CSet1[i][k]][CSet2[j][k-1]][CSet2[j][k]] = StepVec[k];
                                         } else {
                                             StepVec[k] = StepLUT[CSet1[i][k-1]][CSet1[i][k]][CSet2[j][k-1]][CSet2[j][k]];
                                         }
                                     }
                                     double CurrPhase = std::accumulate(PhaseVec.begin(),PhaseVec.end(),0LL);
                                     double CurrStep = std::accumulate(StepVec.begin(),StepVec.end(),0LL);
                                     if(!(BestPhase < CurrPhase)){
                                         BestPhase = CurrPhase;
                                         BestConsts[0][0] = i;
                                         BestConsts[0][1] = j;
//                                         BestPhaseVec = PhaseVec;
                                     }
                                     if(!(BestStep < CurrStep)){
                                         BestStep = CurrStep;
                                         BestConsts[1][0] = i;
                                         BestConsts[1][1] = j;
//                                         BestStepVec = StepVec;
                                     }
                                 }
                             }
                         } else{
                             // Just PhaSim goes here
                              BestPhase = NAN;
                             for(int i = 0; i<CSet1.size(); i++){
                                     for(int j = 0; j<CSet2.size(); j++){

                                         std::vector<std::vector<double>> tVec1(lcslen,std::vector<double> (2, NAN));

                                         std::vector<std::vector<double>> tVec2(lcslen,std::vector<double> (2, NAN));

                                             std::vector<double> PhaseVec(lcslen, NAN);
                                                 for(int m = 0; m<lcslen; m++){
                                                     std::vector<double> TPair1(2, NAN);
                                                     TPair1[0] = TSeqArr1[CSet1[i][m]];
                                                     TPair1[1] = TSeqDep1[CSet1[i][m]];
                                                     tVec1[m] = TPair1;
                                                     std::vector<double> TPair2(2, NAN);
                                                     TPair2[0] = TSeqArr2[CSet2[j][m]];
                                                     TPair2[1] = TSeqDep2[CSet2[j][m]];
                                                     tVec2[m] = TPair2;
                                                 }
                                             
                                             std::vector<double> DiffVec(lcslen, NAN);
                                         double DiffMean;
                                         for(int k = 0; k<lcslen; k++){
                                                if(!(DiffLUT[CSet1[0][k]][CSet2[0][k]] > 0)){
                                                    DiffVec[k] = ((tVec1[k][0] - tVec2[k][0]) + (tVec1[k][1] - tVec2[k][1]));
                                                    DiffLUT[CSet1[0][k]][CSet2[0][k]] = DiffVec[k];
                                                } else {
                                                    DiffVec[k] = DiffLUT[CSet1[0][k]][CSet2[0][k]];
                                                }
                                         }
                                         
                                         double DiffSum = std::accumulate(DiffVec.begin(), DiffVec.end(), 0LL);
                                         DiffMean = DiffSum / (2 * DiffVec.size());
                                         
                                     for(int k = 0; k<lcslen; k++){
                                         PhaseVec[k] = (std::pow((tVec1[k][0]-tVec2[k][0] - DiffMean), 2)  +  std::pow((tVec1[k][1]-tVec2[k][1] - DiffMean), 2));
                                         
                                     }
                                     double CurrPhase = std::accumulate(PhaseVec.begin(),PhaseVec.end(),0LL);
                                     if(!(BestPhase < CurrPhase)){
                                         BestPhase = CurrPhase;
                                         BestConsts[0][0] = i;
                                         BestConsts[0][1] = j;
//                                         BestPhaseVec = PhaseVec;
                                     }
                                     }
                                 }
                             }
                     } else if(std::find(Method.begin(), Method.end(), "LeastStep") != Method.end()){
                         // Just LeastStep goes here
                          BestStep = NAN;
                         for(int i = 0; i<CSet1.size(); i++){
                                 for(int j = 0; j<CSet2.size(); j++){

                                     std::vector<std::vector<double>> tVec1(lcslen,std::vector<double> (2, NAN));

                                     std::vector<std::vector<double>> tVec2(lcslen,std::vector<double> (2, NAN));

                                         std::vector<double> StepVec(lcslen, NAN);
                                             for(int m = 0; m<lcslen; m++){
                                                 std::vector<double> TPair1(2, NAN);
                                                 TPair1[0] = TSeqArr1[CSet1[i][m]];
                                                 TPair1[1] = TSeqDep1[CSet1[i][m]];
                                                 tVec1[m] = TPair1;
                                                 std::vector<double> TPair2(2, NAN);
                                                 TPair2[0] = TSeqArr2[CSet2[j][m]];
                                                 TPair2[1] = TSeqDep2[CSet2[j][m]];
                                                 tVec2[m] = TPair2;
                                             }
                                     
                                     StepVec[0] = (0);
                                     
                                 for(int k = 1; k<lcslen; k++){
                                     if(!(StepLUT[CSet1[i][k-1]][CSet1[i][k]][CSet2[j][k-1]][CSet2[j][k]] > 0)){
                                     StepVec[k] = ((std::pow((tVec1[k][0]-tVec1[k-1][1]) - (tVec2[k][0]-tVec2[k-1][1]),2)/2)  + (std::pow((tVec1[k][1]-tVec1[k][0]) - (tVec2[k][1]-tVec2[k][0]), 2)/2));
                                         StepLUT[CSet1[i][k-1]][CSet1[i][k]][CSet2[j][k-1]][CSet2[j][k]] = StepVec[k];
                                     } else {
                                         StepVec[k] = StepLUT[CSet1[i][k-1]][CSet1[i][k]][CSet2[j][k-1]][CSet2[j][k]];
                                     }
                                 }
                                 double CurrStep = std::accumulate(StepVec.begin(),StepVec.end(),0LL);
                                 if(!(BestStep < CurrStep)){
                                     BestStep = CurrStep;
                                     BestConsts[0][0] = i;
                                     BestConsts[0][1] = j;
//                                     BestStepVec = StepVec;
                                 }
                             }
                         }
                         
                     }

                      // By the end of this loop, we have the two best constructors for each sequence

                      std::vector<std::vector<std::vector<std::vector<double>>>> BestPair(l, std::vector<std::vector<std::vector<double>>>(2));
                     for(int f = 0; f < l; f++){
                         // We'd need to build this specific case of Constr Table a lil differently, but it's manageable!

                         
                             std::vector<std::vector<double>> TPairVec1(lcslen, std::vector<double>(2, NAN));
                             
                             for(int j = 0; j<lcslen; j++){
                                 std::vector<double> TPair1(2, NAN);
                                 TPair1[0] = (TSeqArr1[CSet1[BestConsts[f][0]][j]]);
                                 TPair1[1] = (TSeqDep1[CSet1[BestConsts[f][0]][j]]);
                                 TPairVec1[j] = (TPair1);
                             }
                            
                         
                         
                         
                         std::vector<std::vector<double>> TPairVec2(lcslen, std::vector<double>(2, NAN));
                             
                             for(int j = 0; j<CSet2[0].size(); j++){
                                 std::vector<double> TPair2(2, NAN);
                                 TPair2[0] = (TSeqArr2[CSet2[BestConsts[f][1]][j]]);
                                 TPair2[1] = (TSeqDep2[CSet2[BestConsts[f][1]][j]]);
                                 TPairVec2[j] = (TPair2);
                             }
                             
                         
                      BestPair[f][0] = (TPairVec1);
                      BestPair[f][1] = (TPairVec2);
                    }
                      BestConst = BestPair;
                 }
        
        
                 std::vector<std::string> LCSString = LCSStringify(LCSset[i], lcslen, LocLib);
                 
        std::vector<double> OffVec(l);
        // Here we can use find() to define OffVec
        std::vector<double> OffSum(l);
        if(std::find(Method.begin(), Method.end(), "LeastDist") != Method.end()){
            if(std::find(Method.begin(), Method.end(), "PhaSim") != Method.end()){
                if(std::find(Method.begin(), Method.end(), "LeastStep") != Method.end()){
                    

                    OffVec[0] = BestDist / lcslen;

                    OffVec[1] = BestPhase / lcslen;

                    OffVec[2] = BestStep / lcslen;

                } else {

                    OffVec[0] = BestDist / lcslen;

                    OffVec[1] = BestPhase / lcslen;

                }} else if(std::find(Method.begin(), Method.end(), "LeastStep") != Method.end()){


                    OffVec[0] = BestDist / lcslen;

                    OffVec[1] = BestStep / lcslen;
                    
                } else {

                    OffVec[0] = BestDist / lcslen;

                }} else if(std::find(Method.begin(), Method.end(), "PhaSim") != Method.end()){
                    if(std::find(Method.begin(), Method.end(), "LeastStep") != Method.end()){

                        OffVec[0] = BestPhase / lcslen;

                        OffVec[1] = BestStep / lcslen;

                    } else{

                        OffVec[0] = BestPhase / lcslen;

                    }} else if(std::find(Method.begin(), Method.end(), "LeastStep") != Method.end()){

                        OffVec[0] = BestStep / lcslen;
        }
                     //End of the loop through each LCS, save the least squares distance through time for each "best constructor" for each LCS
        for(int k = 0; k < l; k++){

            
                 std::tuple<std::vector<std::string>,std::vector<std::vector<double>>,std::vector<std::vector<double>>,double> CombinedConst = std::make_tuple(LCSString, BestConst[k][0], BestConst[k][1], OffVec[k]);
                 

            Candidates[k].push_back(CombinedConst);
        }
       }
    // This section is good because l breaks it down by method already, which is nice.
    for(int k = 0; k < Method.size(); k++){
    if(Candidates[k].size() != 1){
    std::vector<double> AggDist;
    for(int i = 0; i<Candidates[k].size(); i++){
        AggDist.push_back(std::get<3>(Candidates[k][i]));
    }
        // There's at least one bug in here.
    auto BestTarget = std::min_element(AggDist.begin(), AggDist.end());
    int BestCand = std::distance(AggDist.begin(),BestTarget);
    PullSet.push_back(Candidates[k][BestCand]);
    } else {
        PullSet.push_back(Candidates[k][0]);
    }
    }
    return;
}



std::vector<std::tuple<std::vector<std::string>,std::vector<std::vector<double>>,std::vector<std::vector<double>>,double>> FullLCSExtractor(std::vector<int> Fish1Seq, std::vector<int> Fish2Seq/*, int argc, char** argv*/, std::vector<double> TSeqArr1, std::vector<double> TSeqDep1, std::vector<double> TSeqArr2, std::vector<double> TSeqDep2, std::vector<std::string> Method, std::vector<std::string> LocLib){
    int Size1;
    int Size2;
    
   Size1 = Fish1Seq.size();
   Size2 = Fish2Seq.size();
    
  //std::cout << Size1 << " is the length of the first object.\n";
  //std::cout << Size2 << " is the length of the second object.\n";
    
    std::vector<std::vector<int>> DPmat;
    
    DPmatInit(Size1, Size2, DPmat);
    
    // Preliminary setting/context for running the process
    
    
    // Find length of LCS


        int lcslen = lcscore(Fish1Seq, Fish2Seq, Size1, Size2, 0, 0, DPmat);
    

    //std::cout << lcslen << " is the length of the lcs.\n";
    // Use recursive backtracing to generate all LCS
    // data, F1Index, & F2Index are used for storage
    // Each should be as long as the lcs
    std::vector<int> data(lcslen);
    std::vector<int> F1Index(lcslen);
    std::vector<int> F2Index(lcslen);
    std::vector<std::vector<int>> LCSset;
    
    std::vector<std::tuple<std::vector<std::string>,std::vector<std::vector<double>>,std::vector<std::vector<double>>,double>> PullSet;

    PullAll(Fish1Seq, Fish2Seq, Size1, Size2, data, 0, 0, 0, LCSset, lcslen, DPmat/*PullSet, F1Index, F2Index*/);
    
   // std::cout << "It got this far" << "\n";
    // std::cout << PullSet[0][0][0] << PullSet[0][0][1] << PullSet[0][0][2] << PullSet[0][0][3] << "\n";
    // std::cout << PullSet[0][1][0] << PullSet[0][1][1] << PullSet[0][1][2] << PullSet[0][1][3] << "\n";
    // std::cout << PullSet[0][2][0] << PullSet[0][2][1] << PullSet[0][2][2] << PullSet[0][2][3] << "\n";
    // LCSset now contains every LCS, need to add a utility/pair of utilities for comparing against vectors of strings to find constructors and staple combinations of {vector(LCS),vector(F1Construct),vector(F2Construct)} onto PullSet
    std::vector<std::string> Methods;
    if(Method[0].compare("All") == 0){
      Methods = {"LeastDist","PhaSim","LeastStep"};
    } else{
      Methods = Method;
    }
    LCSConstructHunt(LCSset, Fish1Seq, Fish2Seq, PullSet, TSeqArr1, TSeqDep1, TSeqArr2, TSeqDep2, Methods, LocLib, lcslen, Size1, Size2);
    //ConstructHunt will select one option to output as PullSet, which it will use part of the artist formerly known as LCSStapler to convert the integer LCS into the string LCS for.
    
    return PullSet;
}



std::string Dble2Str(double Dble){
    std::string Out = std::to_string(Dble);
    return(Out);
}

std::vector<std::string> Dble2StrVec(std::vector<double> Dble){
    std::vector<std::string> Out;
    for(int i = 0; i < Dble.size(); i++){
        Out.push_back(std::to_string(Dble[i]));
    }
    return(Out);
}
//
//SEXP LCSWrapper(Rcpp::StringVector Fish1Seqa, Rcpp::StringVector Fish2Seqa, Rcpp::NumericVector TSeqArr1a, Rcpp::NumericVector TSeqDep1a, Rcpp::NumericVector TSeqArr2a, Rcpp::NumericVector TSeqDep2a, Rcpp::StringVector Method_){
//
//    std::vector<std::string> Fish1Seq_ = Rcpp::as<std::vector<std::string>>(Fish1Seqa);
//    std::vector<std::string> Fish2Seq_ = Rcpp::as<std::vector<std::string>>(Fish2Seqa);
//    std::vector<double> TSeqArr1 = Rcpp::as<std::vector<double>>(TSeqArr1a);
//    std::vector<double> TSeqDep1 = Rcpp::as<std::vector<double>>(TSeqDep1a);
//    std::vector<double> TSeqArr2 = Rcpp::as<std::vector<double>>(TSeqArr2a);
//    std::vector<double> TSeqDep2 = Rcpp::as<std::vector<double>>(TSeqDep2a);
//    std::vector<std::string> Method = Rcpp::as<std::vector<std::string>>(Method_);
//
//    std::vector<std::string> LocLib;
//    IntLibConverter(Fish1Seq_,Fish2Seq_,Fish1Seq,Fish2Seq,LocLib);
//    //std::string dbm = as<String>(Debug);
//   /* Rcpp::CharacterVector DbM(Debug);*/
//  //  Rcpp::Rcout << Fish1Seq[0] << Fish1Seq[1] << Fish1Seq[2] << Fish1Seq[3] <<  "\n";
//
//
//    // step 1: call the underlying C++ function
//    std::vector<std::tuple<std::vector<std::string>,std::vector<std::vector<double>>,std::vector<std::vector<double>>,std::vector<double>>> LeastComm = FullLCSExtractor(Fish1Seq, Fish2Seq, TSeqArr1, TSeqDep1, TSeqArr2, TSeqDep2, Method);
//
//    //Convert all the objects in the tuple over to a uniform d vector of strings.
//    //TestPrint(LeastComm);
//
//    std::vector<std::vector<std::string>> LCSPrinted;
//
//    std::vector<std::string> Methods;
//
//    if(Method[0].compare("All") == 0){
//      Methods = {"LeastDist","PhaSim","LeastStep"};
//    } else{
//      Methods = Method;
//    }
//
//    for(int k = 0; k < Methods.size(); k++){
//
//    std::vector<std::string> MethTag;
//
//    MethTag.push_back(Methods[k]);
//
//    std::vector<std::string> LCSStringOut;
//    LCSStringOut = std::get<0>(LeastComm[k]);
//
//    std::vector<std::string> LCSArr1;
//    std::vector<std::string> LCSDep1;
//
//    std::vector<std::string> LCSArr2;
//    std::vector<std::string> LCSDep2;
//
//    std::vector<std::string> LCSDist;
//    LCSDist.resize(std::get<3>(LeastComm[k]).size());
//
//        // Use transform and a little helper function to convert all our double vectors to string
//        std::vector<std::vector<std::string>> ArrDepVec;
//        ArrDepVec.resize(std::get<1>(LeastComm[k]).size());
//
//        std::transform(std::get<1>(LeastComm[k]).begin(),std::get<1>(LeastComm[k]).end(),ArrDepVec.begin(),Dble2StrVec);
//        for(int i = 0; i < ArrDepVec.size(); i++){
//            LCSArr1.push_back(ArrDepVec[i][0]);
//            LCSDep1.push_back(ArrDepVec[i][1]);
//        }
//
//        std::transform(std::get<2>(LeastComm[k]).begin(),std::get<2>(LeastComm[k]).end(),ArrDepVec.begin(),Dble2StrVec);
//    for(int i = 0; i < ArrDepVec.size(); i++){
//        LCSArr2.push_back(ArrDepVec[i][0]);
//        LCSDep2.push_back(ArrDepVec[i][1]);
//    }
//
//
//    std::transform(std::get<3>(LeastComm[k]).begin(),std::get<3>(LeastComm[k]).end(),LCSDist.begin(),Dble2Str);
//
//
//     //   Rcpp::Rcout << MethTag[0] << "\n";
//    for(int i = 0; i < ArrDepVec.size(); i++){
//
//    }
//    // Staple together our new lil mess of a string vector vector.
//    LCSPrinted.push_back(MethTag);
//    LCSPrinted.push_back(LCSStringOut);
//    LCSPrinted.push_back(LCSArr1);
//    LCSPrinted.push_back(LCSDep1);
//    LCSPrinted.push_back(LCSArr2);
//    LCSPrinted.push_back(LCSDep2);
//    LCSPrinted.push_back(LCSDist);
//
//
//    }
//
//    // step 2: return the result as a SEXP
//    return Rcpp::wrap(LCSPrinted);
//}

// [[Rcpp::export]]
SEXP LCSExtract(SEXP Fish1Sequence, SEXP Fish2Sequence, SEXP TSeqArr1_, SEXP TSeqDep1_, SEXP TSeqArr2_, SEXP TSeqDep2_, SEXP MethVec){
    // step 0: convert input to C++ types
    /*std::vector<std::string>>*/Rcpp::StringVector Fish1Seqa(Rcpp::clone(Fish1Sequence));
    /*std::vector<std::string>>*/Rcpp::StringVector Fish2Seqa(Rcpp::clone(Fish2Sequence));
    Rcpp::StringVector Method_(Rcpp::clone(MethVec));
    
    Rcpp::NumericVector TSeqArr1a(Rcpp::clone(TSeqArr1_));
    Rcpp::NumericVector TSeqDep1a(Rcpp::clone(TSeqDep1_));
    
    Rcpp::NumericVector TSeqArr2a(Rcpp::clone(TSeqArr2_));
    Rcpp::NumericVector TSeqDep2a(Rcpp::clone(TSeqDep2_));
    
    
    std::vector<std::string> Fish1Seq_ = Rcpp::as<std::vector<std::string>>(Fish1Seqa);
    std::vector<std::string> Fish2Seq_ = Rcpp::as<std::vector<std::string>>(Fish2Seqa);
    std::vector<double> TSeqArr1 = Rcpp::as<std::vector<double>>(TSeqArr1a);
    std::vector<double> TSeqDep1 = Rcpp::as<std::vector<double>>(TSeqDep1a);
    std::vector<double> TSeqArr2 = Rcpp::as<std::vector<double>>(TSeqArr2a);
    std::vector<double> TSeqDep2 = Rcpp::as<std::vector<double>>(TSeqDep2a);
    std::vector<std::string> Method = Rcpp::as<std::vector<std::string>>(Method_);
                               
    std::vector<std::string> LocLib;
    
    std::vector<int> Fish1Seq;
    std::vector<int> Fish2Seq;
    
    IntLibConverter(Fish1Seq_,Fish2Seq_,Fish1Seq,Fish2Seq,LocLib);
    //std::string dbm = as<String>(Debug);
   /* Rcpp::CharacterVector DbM(Debug);*/
  //  Rcpp::Rcout << Fish1Seq[0] << Fish1Seq[1] << Fish1Seq[2] << Fish1Seq[3] <<  "\n";

    
    // step 1: call the underlying C++ function
    std::vector<std::tuple<std::vector<std::string>,std::vector<std::vector<double>>,std::vector<std::vector<double>>,double>> LeastComm = FullLCSExtractor(Fish1Seq, Fish2Seq, TSeqArr1, TSeqDep1, TSeqArr2, TSeqDep2, Method, LocLib);

    //Convert all the objects in the tuple over to a uniform d vector of strings.
    //TestPrint(LeastComm);
                               
    std::vector<std::vector<std::string>> LCSPrinted;
    
    std::vector<std::string> Methods;
    
    if(Method[0].compare("All") == 0){
      Methods = {"LeastDist","PhaSim","LeastStep"};
    } else{
      Methods = Method;
    }
                               
    for(int k = 0; k < Methods.size(); k++){
        
    std::vector<std::string> MethTag;
        
    MethTag.push_back(Methods[k]);
        
    std::vector<std::string> LCSStringOut;
        
    LCSStringOut = std::get<0>(LeastComm[k]);
    
    std::vector<std::string> LCSArr1;
    std::vector<std::string> LCSDep1;

    std::vector<std::string> LCSArr2;
    std::vector<std::string> LCSDep2;
    
    std::vector<std::string> LCSDist;
        
        // Use transform and a little helper function to convert all our double vectors to string
        std::vector<std::vector<std::string>> ArrDepVec;
        
        
        ArrDepVec.clear();
        ArrDepVec.resize(std::get<1>(LeastComm[k]).size());
        
        LCSArr1.clear();
        LCSDep1.clear();
        std::transform(std::get<1>(LeastComm[k]).begin(),std::get<1>(LeastComm[k]).end(),ArrDepVec.begin(),Dble2StrVec);
        for(int i = 0; i < LCSStringOut.size(); i++){
            LCSArr1.push_back(ArrDepVec[i][0]);
            LCSDep1.push_back(ArrDepVec[i][1]);
        }
        
        ArrDepVec.clear();
        ArrDepVec.resize(std::get<1>(LeastComm[k]).size());
        
        LCSArr2.clear();
        LCSDep2.clear();

        std::transform(std::get<2>(LeastComm[k]).begin(),std::get<2>(LeastComm[k]).end(),ArrDepVec.begin(),Dble2StrVec);
    for(int i = 0; i < LCSStringOut.size(); i++){
        LCSArr2.push_back(ArrDepVec[i][0]);
        LCSDep2.push_back(ArrDepVec[i][1]);
    }
    
    
        LCSDist.push_back(Dble2Str(std::get<3>(LeastComm[k])));
    
     //   Rcpp::Rcout << MethTag[0] << "\n";
    for(int i = 0; i < ArrDepVec.size(); i++){
            
    }
    // Staple together our new lil mess of a string vector vector.
    LCSPrinted.push_back(MethTag);
    LCSPrinted.push_back(LCSStringOut);
    LCSPrinted.push_back(LCSArr1);
    LCSPrinted.push_back(LCSDep1);
    LCSPrinted.push_back(LCSArr2);
    LCSPrinted.push_back(LCSDep2);
    LCSPrinted.push_back(LCSDist);
        
    
    }
    
    return Rcpp::wrap(LCSPrinted);
    /*
    SEXP LCSROut = LCSWrapper(Fish1Seqa, Fish2Seqa, TSeqArr1a, TSeqDep1a, TSeqArr2a, TSeqDep2a, Method_);
    
    return LCSROut;
     */
}
