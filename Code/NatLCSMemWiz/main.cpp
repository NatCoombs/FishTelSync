//
//  LCSMemWizCpp.cpp
//
//
//  Created by Nat Coombs on 4/6/22.
//  True exhaustive search version
//
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
    std::cout << "The number of nodes visited is: ";
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
                    std::cout << NodeInc << " ";
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
    //int NodeInc = 0;
    int MaxDepth = lcslen - 1;
    for(int i = 0; i < StartKeys.size(); i++){
            
            std::stack<int> s;
            
            s.push(StartKeys[i]);
        
            while(!s.empty()){
                //std::cout << "The stack is: " << s.size() << " elements long.\n";
                int Curr = s.top();
                s.pop();
//                auto dInd = std::find(DKey[0].begin(), DKey[0].end(), Curr);
//
//                int Depth = DKey[1][dInd - DKey[0].begin()];
                int Depth = DKey[1][Curr];
                
               
                    //NodeInc++;
                    //std::cout << NodeInc << " ";
                    //std::cout << "The most recent new node visited is: " <<Curr << "\n";
                    
                    //visited[Curr] = true;
                    p[Depth] = Curr;
                
                    if(Depth == MaxDepth){
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
    std::cout << dodec << " is the number of partitions to be included in the constructor set.\nThe current number of partitions evaluated is: ";
    
    std::vector<std::vector<std::vector<int>>> PathComp(dodec);
    for(int i = 0; i < end; i++){
        std::vector<std::vector<int>> Parti;
        int lB = i*PartDex;
        int uB = (i+1)*PartDex;
        
        Parti = DnCSub(Graph, DKey, uB, lB);
        std::cout << i + 1 << " ";
        
        
        PathComp[i] = Parti;
    }
    int lB = end*PartDex;
    int uB = MaxDepth;
    
    PathComp[end] = DnCSub(Graph, DKey, uB, lB);
    std::cout << dodec << "\n";
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
    
    std::cout << "The current number of partitions in the constructor set is: 1 ";
    
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
        std::cout << i + 1 << " ";
    }
    // At the end of the loop, F1 is the one we want to unravel into each constructor
    for(int i = 0; i < DdexPaths[F1].size(); i++){
        for(int j = 0; j < DdexPaths[F1][i].size(); j++){
            CSet.push_back(DdexPaths[F1][i][j]);
        }
    }
    std::cout << "\n";
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




void LCSPrinter2(std::vector<std::tuple<std::vector<std::string>,std::vector<std::vector<double>>,std::vector<std::vector<double>>,double>> LCSOut){
    for(int n = 0; n < LCSOut.size(); n++){
    for(int k = 0; k < std::get<0>(LCSOut[n]).size(); k++){
        std::cout << std::get<0>(LCSOut[n])[k] << " ";
    }
    std::cout << "\n";
    for(int k = 0; k < std::get<0>(LCSOut[n]).size(); k++){
        std::cout << std::get<1>(LCSOut[n])[k][0] << " ";
    }
    std::cout << "\n";
    for(int k = 0; k < std::get<0>(LCSOut[n]).size(); k++){
        std::cout << std::get<1>(LCSOut[n])[k][1] << " ";
    }
    std::cout << "\n";
    for(int k = 0; k < std::get<0>(LCSOut[n]).size(); k++){
        std::cout << std::get<2>(LCSOut[n])[k][0] << " ";
    }
    std::cout << "\n";
    for(int k = 0; k < std::get<0>(LCSOut[n]).size(); k++){
        std::cout << std::get<2>(LCSOut[n])[k][1] << " ";
    }
    std::cout << "\n";
        
        std::cout << std::get<3>(LCSOut[n]) << " ";
    
    std::cout << "\n";
    }
}
int main(int argc, char** argv){
    //StringVecBuilder(argv, 1, "Switch", "Stop", StringIndex, Fish1Seq_, Fish2Seq_/*,TSeq1,TSeq2*/);
    // Initialize the fishseq_'s and tvec's here:
    std::vector<std::string> Fish1Seq_ { "ColemanRel", "Battle_Ck", "SR_JellysFerry", "Battle_Ck", "SR_JellysFerry", "Battle_Ck", "SR_SturgeonHole", "Battle_Ck", "SR_SturgeonHole", "Battle_Ck", "SR_SturgeonHole", "Battle_Ck", "SR_SturgeonHole", "Battle_Ck", "SR_SturgeonHole", "Battle_Ck", "SR_SturgeonHole", "Battle_Ck", "SR_SturgeonHole", "Battle_Ck", "SR_SturgeonHole", "Battle_Ck", "SR_SturgeonHole", "Battle_Ck", "SR_SturgeonHole", "Battle_Ck", "SR_SturgeonHole", "Battle_Ck", "SR_BoulderHole", "Battle_Ck", "SR_BendBridge", "Battle_Ck", "SR_BendBridge", "Battle_Ck", "SR_BendBridge", "Battle_Ck", "SR_BendBridge", "Battle_Ck", "SR_ChinaRapids", "SR_RedBluff", "SR_BlwRBDD", "SR_SaltCk", "SR_AbvAntelopeCk", "SR_BlwAntelopeCk", "SR_AbvThomes", "SR_AbvToomes", "Woodson Temp", "SR_AbvGCID", "SR_BlwGCID", "SR_BlwIrvineFinch", "SR_AbvOrd", "SR_BlwOrd", "SR_ButteCityBr", "SR_BlwButte", "SR_AbvColusaBr", "SR_AbvTisdale", "SR_BlwChinaBend", "AlcatrazSW", "SR_KnightsLanding", "SR_AbvFeather", "FR_Verona", "SR_I-80/50Br", "SR_GB", "SR_Freeport", "SR_GB", "SR_KK", "SR_BlwSteam", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_BlwGeorgiana", "SR_KK", "SR_RV", "SR_Mouth", "SR_RV", "SR_RioVista", "SR_RV", "SR_RioVista", "SR_RV", "SR_RioVista", "SR_RV", "Decker_IsN", "ThreeMile", "SJ_Antioch", "Chipps Island", "SP_Buoy", "SP_Control", "Richmond Bridge", "Bay Bridge", "Pt_Reyes" };
    std::vector<std::string> Fish2Seq_ { "ColemanRel", "Battle_Ck", "Massacre Flat", "Battle_Ck", "SR_SturgeonHole", "Battle_Ck", "SR_BendBridge", "Battle_Ck", "SR_ChinaRapids", "SR_RedBluff", "SR_SaltCk", "SR_AbvThomes", "Woodson Temp", "SR_AbvGCID", "SR_BlwGCID", "SR_BlwIrvineFinch", "SR_AbvOrd", "SR_BlwOrd", "SR_AbvOrd", "SR_BlwOrd", "SR_AbvOrd", "SR_BlwOrd", "SR_AbvOrd", "SR_BlwOrd", "SR_AbvOrd", "SR_BlwOrd", "SR_AbvOrd", "SR_BlwOrd", "SR_ButteCityBr", "SR_BlwButte", "SR_AbvColusaBr", "SR_MeridianBr", "SR_AbvTisdale", "SR_BlwChinaBend", "AlcatrazSW", "SR_KnightsLanding", "SR_AbvFeather", "FR_Verona", "SR_I-80/50Br", "SR_GB", "SR_Freeport", "SR_GB", "SR_KK", "SR_BlwSteam", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "Georg_SloughN", "SR_KK", "Georg_SloughN", "SR_KK", "SR_DCC", "Georg_SloughN", "SR_BlwGeorgiana", "SR_KK", "SR_BlwGeorgiana", "SR_KK", "SR_RV", "SR_Mouth", "SR_RV", "SR_SteamboatSl", "SR_RioVista", "SR_RV", "SR_RioVista", "SR_RV", "Decker_IsN", "Decker_IsS", "SJ_Antioch", "Chipps Island", "SJ_Antioch", "SJ_JerseyPoint", "Mok_Will_Berm_Mar", "Mok_GeorgianaS", "Georg_SloughS", "Mok_GeorgianaS", "Georg_SloughS", "Georg_SloughN", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_BlwSteam", "CarSt", "SR_SteamboatSl", "SR_SutterSl", "Miner Slough", "SR_RV", "SR_RioVista", "SR_RV", "SR_RioVista", "SR_RV", "Decker_IsN", "SR_RV", "Decker_IsN", "Decker_IsS", "Chipps Island", "Benicia Bridge", "Carquinez Bridge", "SP_Control", "SP_Array", "SP_Control", "SP_Array", "SP_Control", "SP_Array", "Richmond Bridge", "SP_Array", "SP_Control", "SP_Buoy", "Mare_Island", "SP_Control", "SP_Array", "SP_Flats_Array", "SP_Control", "SP_Buoy", "Mare_Island", "Vallejo Marina" };
    std::vector<double> TSeqArr1 { 1260974820, 1261035234, 1261184036, 1261184038, 1261184082, 1261184085, 1261190174, 1261190196, 1261190252, 1261190256, 1261190294, 1261190304, 1261190352, 1261190352, 1261190446, 1261190473, 1261190477, 1261190493, 1261190520, 1261190536, 1261190556, 1261190562, 1261190581, 1261190593, 1261190610, 1261190638, 1261190650, 1261190677, 1261191927, 1261192000, 1261198870, 1261198891, 1261198913, 1261198925, 1261198975, 1261199020, 1261199048, 1261199065, 1261209367, 1261217483, 1261225354, 1261228675, 1261309910, 1261311390, 1261374752, 1261381376, 1261387898, 1261642661, 1261646740, 1261671621, 1261870871, 1262023314, 1262229925, 1262549972, 1262584447, 1262662025, 1262710533, 1262745952, 1262750129, 1262770563, 1262776210, 1262867484, 1263585529, 1263605822, 1263609947, 1263646991, 1263659880, 1263692090, 1263692471, 1263692494, 1263692512, 1263692523, 1263692585, 1263692590, 1263692625, 1263692777, 1263692777, 1263692859, 1263692916, 1263693328, 1263694922, 1263697803, 1263779012, 1263782741, 1263785004, 1263792963, 1263794511, 1263803398, 1263803793, 1263918334, 1263937971, 1263959156, 1264030244, 1264259044, 1264308108, 1264532916, 1264534473, 1264539937, 1264557823, 1265309711 };
    std::vector<double> TSeqDep1 { 1260974820, 1261184004, 1261184036, 1261184038, 1261184082, 1261190173, 1261190196, 1261190222, 1261190252, 1261190256, 1261190294, 1261190304, 1261190352, 1261190439, 1261190446, 1261190473, 1261190477, 1261190514, 1261190520, 1261190536, 1261190556, 1261190562, 1261190581, 1261190593, 1261190610, 1261190638, 1261190650, 1261191913, 1261191961, 1261198865, 1261198870, 1261198891, 1261198913, 1261198973, 1261198975, 1261199020, 1261199048, 1261206855, 1261209655, 1261217829, 1261225354, 1261229116, 1261309910, 1261312156, 1261375536, 1261381428, 1261388782, 1261643397, 1261646933, 1261697227, 1261871140, 1262052352, 1262230831, 1262550277, 1262584595, 1262662025, 1262710624, 1262746174, 1262750129, 1262771090, 1262776287, 1262868075, 1263602741, 1263606352, 1263614463, 1263650678, 1263660810, 1263692455, 1263692471, 1263692494, 1263692512, 1263692523, 1263692585, 1263692590, 1263692744, 1263692777, 1263692777, 1263692859, 1263693117, 1263693328, 1263695151, 1263703992, 1263779519, 1263783498, 1263785085, 1263793239, 1263803174, 1263803639, 1263803958, 1263918334, 1263952487, 1263959324, 1264031220, 1264259044, 1264356251, 1264532916, 1264534523, 1264539937, 1264574214, 1268773232 };
    std::vector<double> TSeqArr2 { 1260974820, 1261088288, 1261465390, 1261466144, 1261467140, 1261472340, 1261475858, 1261476258, 1261486726, 1261493359, 1261531101, 1261549955, 1261562050, 1261635724, 1261640109, 1261651703, 1261929971, 1261939708, 1261939735, 1261939750, 1261939777, 1261939784, 1261939813, 1261939823, 1261939835, 1261939844, 1261939861, 1261968565, 1262041573, 1262215733, 1262300926, 1262422640, 1262450928, 1262502339, 1262534801, 1262541045, 1262709742, 1262714692, 1262781070, 1262964438, 1263048924, 1263075935, 1263121042, 1263134341, 1263215715, 1263255258, 1263255286, 1263255374, 1263255403, 1263255413, 1263255448, 1263255484, 1263255488, 1263255614, 1263255633, 1263255660, 1263302455, 1263302480, 1263302580, 1263302580, 1263302750, 1263302750, 1263302772, 1263302795, 1263302827, 1263302864, 1263302868, 1263302933, 1263302942, 1263302965, 1263302988, 1263303009, 1263303056, 1263303119, 1263303126, 1263303151, 1263303157, 1263303197, 1263303201, 1263303240, 1263303241, 1263303278, 1263303343, 1263303426, 1263303470, 1263303492, 1263303574, 1263303690, 1263303857, 1263303934, 1263303959, 1263304037, 1263304081, 1263304176, 1263304206, 1263304208, 1263304230, 1263304253, 1263304259, 1263304377, 1263304400, 1263304571, 1263304618, 1263304619, 1263304666, 1263305588, 1263305603, 1263306782, 1263306819, 1263407179, 1263407206, 1263407307, 1263407328, 1263407373, 1263407376, 1263407558, 1263407570, 1263407619, 1263407628, 1263407740, 1263407755, 1263407786, 1263407816, 1263407866, 1263407879, 1263408019, 1263408026, 1263408062, 1263408063, 1263408099, 1263408216, 1263408257, 1263408259, 1263408306, 1263409056, 1263409069, 1263409255, 1263409396, 1263409422, 1263409704, 1263409786, 1263420308, 1263945091, 1263948821, 1264048265, 1264051319, 1264052623, 1264075867, 1264733782, 1264734329, 1264759046, 1264760813, 1264781683, 1264821514, 1264975921, 1265001897, 1265062823, 1265192864, 1265216707, 1265230910, 1265238172, 1265258294, 1265326206, 1265469783, 1265472987, 1265472999, 1265473043, 1265473287, 1265473353, 1265473481, 1265473538, 1265473569, 1265473610, 1265473723, 1265493563, 1265507915, 1265514411, 1265648705, 1265681730, 1265755763, 1265756308, 1265756598, 1265756666, 1265760179, 1265803173, 1265831304, 1265838495, 1265847468, 1265937164, 1266010091, 1266048695, 1266110270, 1266111599, 1266152348, 1266159039, 1266185473, 1266187870, 1266197942, 1266210203, 1266213252, 1266218298, 1266264615, 1266285677, 1266287859, 1266297788, 1266325316, 1266345896, 1266439380, 1266465537 };
    std::vector<double> TSeqDep2 { 1260974820, 1261450629, 1261465390, 1261466311, 1261467278, 1261473839, 1261475954, 1261476258, 1261487078, 1261493359, 1261531288, 1261550093, 1261562423, 1261636803, 1261640286, 1261652566, 1261939704, 1261939708, 1261939735, 1261939750, 1261939777, 1261939803, 1261939813, 1261939823, 1261939835, 1261939844, 1261954239, 1261969136, 1262042833, 1262216065, 1262301048, 1262448038, 1262450928, 1262502493, 1262535339, 1262541439, 1262710309, 1262715603, 1262781265, 1263045220, 1263058183, 1263081097, 1263123635, 1263158593, 1263255240, 1263255258, 1263255331, 1263255374, 1263255403, 1263255413, 1263255448, 1263255484, 1263255488, 1263255614, 1263255633, 1263302435, 1263302455, 1263302558, 1263302580, 1263302676, 1263302750, 1263302771, 1263302772, 1263302824, 1263302827, 1263302864, 1263302922, 1263302933, 1263302964, 1263302965, 1263302988, 1263303009, 1263303056, 1263303119, 1263303126, 1263303151, 1263303157, 1263303197, 1263303201, 1263303240, 1263303241, 1263303336, 1263303389, 1263303464, 1263303470, 1263303529, 1263303684, 1263303690, 1263303914, 1263303934, 1263304034, 1263304066, 1263304127, 1263304176, 1263304206, 1263304208, 1263304230, 1263304253, 1263304328, 1263304377, 1263304520, 1263304571, 1263304618, 1263304619, 1263305562, 1263305588, 1263306761, 1263306782, 1263407130, 1263407179, 1263407295, 1263407307, 1263407328, 1263407373, 1263407532, 1263407558, 1263407595, 1263407619, 1263407717, 1263407740, 1263407782, 1263407786, 1263407856, 1263407866, 1263407983, 1263408019, 1263408026, 1263408062, 1263408089, 1263408208, 1263408216, 1263408257, 1263408259, 1263408629, 1263409056, 1263409069, 1263409255, 1263409396, 1263409485, 1263409704, 1263416882, 1263943010, 1263946822, 1263953342, 1264049568, 1264051891, 1264053601, 1264688639, 1264734285, 1264756409, 1264760562, 1264771566, 1264803678, 1264897898, 1264987697, 1265051793, 1265100530, 1265192999, 1265220672, 1265231438, 1265238227, 1265281789, 1265387771, 1265472586, 1265472987, 1265472999, 1265473270, 1265473342, 1265473473, 1265473481, 1265473538, 1265473569, 1265473610, 1265475054, 1265493563, 1265511332, 1265514642, 1265663463, 1265742777, 1265756255, 1265756365, 1265756598, 1265756666, 1265795989, 1265805070, 1265832735, 1265839162, 1265893110, 1265937861, 1266011719, 1266096863, 1266111406, 1266116871, 1266156373, 1266166573, 1266186751, 1266190255, 1266197942, 1266212187, 1266213941, 1266218803, 1266267917, 1266285964, 1266289503, 1266300674, 1266340979, 1266360409, 1266440142, 1266466374 };
    
    std::vector<std::string> Method = {"All"};
    // Methods may include LeastDist, PhaSim, and LeastStep, or All for all three in that order.
    std::vector<std::string> LocLib;
    
    std::vector<int> Fish1Seq;
    std::vector<int> Fish2Seq;
    
    IntLibConverter(Fish1Seq_,Fish2Seq_,Fish1Seq,Fish2Seq,LocLib);
    // This may need to be adjusted so that we can more easily print out each possible options. Maybe just a methods arg for each?
    // How about this: we set up LCSOut to be a vector of tuples of length equal to the number of methods, and, inside the loop, we end up pulling
    std::vector<std::tuple<std::vector<std::string>,std::vector<std::vector<double>>,std::vector<std::vector<double>>,double>> LCSOut;
    // The new LCSout needs to be a tuple containing a vector of strings for the LCS, two vectors of pairs of doubles for the constructors, and a vector of floats for the parameter of interest (distance, for now)
    LCSOut = FullLCSExtractor(Fish1Seq, Fish2Seq, /*argc, argv*/ TSeqArr1, TSeqDep1, TSeqArr2, TSeqDep2, Method, LocLib);
    
    // LCSPrinter2 currently only works with the output of one task (Least Distance, Phase Similarity, Min Step Offset), so we need a method for taking multiple inputs (loop through a vector for LCSOut?)
    LCSPrinter2(LCSOut);
    return 0;
}

/* Sets of inputs for testing:
 Case 1:
 
 std::vector<std::string> Fish1Seq_ {"Alpha","Beta","Charlie","Delta"};
 std::vector<std::string> Fish2Seq_ {"Alpha","Charlie","Beta","Delta"};
 std::vector<double> TSeqArr1 {0,2,4,6};
 std::vector<double> TSeqDep1 {1,3,5,7};
 std::vector<double> TSeqArr2 {0,4,5,8};
 std::vector<double> TSeqDep2 {1,4.5,6,12};
 
 Case 2:
 
 std::vector<std::string> Fish1Seq_ {"1","3","2","4"};
 std::vector<std::string> Fish2Seq_ {"1","2","3","4"};
 std::vector<double> TSeqArr1 {0,2,4,6};
 std::vector<double> TSeqDep1 {1,3,5,7};
 std::vector<double> TSeqArr2 {0,4,5,8};
 std::vector<double> TSeqDep2 {1,4.5,6,12};
 
 Case 3:
 
 std::vector<std::string> Fish1Seq_ {"A","B","A","B"};
 std::vector<std::string> Fish2Seq_ {"A","B"};
 std::vector<double> TSeqArr1 {0,2,4,6};
 std::vector<double> TSeqDep1 {1,3,5,7};
 std::vector<double> TSeqArr2 {0,4};
 std::vector<double> TSeqDep2 {1,4.5};
 
 Case 4:
 
 std::vector<std::string> Fish1Seq_ {"A","B","C","B","C","D"};
 std::vector<std::string> Fish2Seq_ {"A","B","C","D"};
 std::vector<double> TSeqArr1 {0,2,4,4.5,6,8};
 std::vector<double> TSeqDep1 {1,3,4.3,5,7,9};
 std::vector<double> TSeqArr2 {0,2,4,6};
 std::vector<double> TSeqDep2 {1,3,5,7};
 
 Case 5:
 
 std::vector<std::string> Fish1Seq_ {"A","B","C","B","C","D"};
 std::vector<std::string> Fish2Seq_ {"A","B","C","D","B","C","D"};
 std::vector<double> TSeqArr1 {0,2,2.5,4,4.5,6};
 std::vector<double> TSeqDep1 {1,2.25,3,4.25,5,7};
 std::vector<double> TSeqArr2 {0,2,4,6,8,10,12};
 std::vector<double> TSeqDep2 {1,3,5,7,9,11,13};
 
 Case 6:
 
 std::vector<std::string> Fish1Seq_ {"A","B","C","B","C","D","B","C","D"};
 std::vector<std::string> Fish2Seq_ {"A","B","C","D","B","C","D","C","B","D"};
 std::vector<double> TSeqArr1 {0,2,2.5,4,4.5,6,8,10,12};
 std::vector<double> TSeqDep1 {1,2.25,3,4.25,5,7,9,11,13};
 std::vector<double> TSeqArr2 {0,2,4,6,8,10,12,14,16,18};
 std::vector<double> TSeqDep2 {1,3,5,7,9,11,13,15,17,19};
 
 Case 7:
 
 std::vector<std::string> Fish1Seq_ {"A","B","C","D"};
 std::vector<std::string> Fish2Seq_ {"a","C","B","D"};
 std::vector<double> TSeqArr1 {0,2,4,6};
 std::vector<double> TSeqDep1 {1,3,5,7};
 std::vector<double> TSeqArr2 {0,4,5,8};
 std::vector<double> TSeqDep2 {1,4.5,6,12};
 
 Case Bugged in R:
 
 std::vector<std::string> Fish1Seq_ {"A","B","A","B","C","B","C","E","D"};
 std::vector<std::string> Fish2Seq_ {"A","B","C","B","D","B","C","D","E"};
 std::vector<double> TSeqArr1 {0,2,4,4.5,6,8,10,12,14};
 std::vector<double> TSeqDep1 {1,3,4.3,5,7,9,11,13,15};
 std::vector<double> TSeqArr2 {0,2,4,6,8,10,12,14,16};
 std::vector<double> TSeqDep2 {1,3,5,7,9,11,13,15,17};
 
 Test Case Real Current a/o 1/25/22:
 
 std::vector<std::string> Fish1Seq_ { "Battle_Ck", "SR_JellysFerry", "SR_ChinaRapids", "SR_RedBluff", "SR_BlwRBDD", "SR_AbvThomes", "SR_BlwGCID", "SR_BlwIrvineFinch", "SR_AbvChicoCk", "SR_BlwIrvineFinch", "SR_AbvChicoCk", "SR_OrdBend", "SR_ButteCityBr", "SR_AbvColusaBr", "SR_MeridianBr", "SR_I-80/50Br", "SR_Freeport", "SR_SutterSl", "SR_DCC", "Georg_SloughN", "SR_Ryde", "SR_Mouth", "SR_RioVista", "HorseshoeBend", "Decker_IsS", "Chipps Island", "Benicia Bridge", "Carquinez Bridge", "SR_JellysFerry" };
 std::vector<std::string> Fish2Seq_ { "Battle_Ck", "SR_JellysFerry", "SR_ChinaRapids", "SR_RedBluff", "SR_AbvThomes", "SR_BlwDryCk", "SR_BlwGCID", "SR_BlwIrvineFinch", "SR_AbvChicoCk", "SR_OrdBend", "SR_ButteCityBr", "SR_AbvColusaBr", "SR_MeridianBr", "SR_I-80/50Br", "SR_Freeport", "SR_SutterSl", "SR_DCC", "Georg_SloughN", "SR_Ryde", "SR_Mouth", "SR_RioVista", "Decker_IsS", "SJ_Antioch", "Chipps Island", "Benicia Bridge", "Carquinez Bridge", "SFPiers", "Golden Gate and Ocean" };
 std::vector<double> TSeqArr1 { 1178684528, 1180035014, 1180271780, 1180389413, 1180433016, 1180661044, 1181300701, 1181348960, 1181350323, 1181350367, 1181350389, 1181397686, 1181431111, 1181478838, 1181491718, 1181628811, 1181654251, 1181684456, 1181731613, 1181733848, 1181736480, 1181746177, 1181750179, 1181765124, 1181776753, 1181817551, 1181828894, 1181865945, 1190965840 };
 std::vector<double> TSeqDep1 { 1178685145, 1180048856, 1180313031, 1180398477, 1180433067, 1180669990, 1181302822, 1181349709, 1181350323, 1181350367, 1181365730, 1181398550, 1181431345, 1181478903, 1181491864, 1181629442, 1181659196, 1181723558, 1181732622, 1181733886, 1181737152, 1181746177, 1181752104, 1181771200, 1181810158, 1181819647, 1181860180, 1181876939, 1190965840 };
 std::vector<double> TSeqArr2 { 1178611017, 1178671176, 1179314049, 1179328730, 1179447575, 1179450368, 1179477827, 1179500487, 1179501829, 1179528125, 1179549840, 1179597976, 1179610230, 1179726034, 1179759130, 1179779044, 1179785144, 1179796087, 1179814662, 1179846596, 1179850894, 1179859637, 1179891851, 1179935241, 1179949563, 1179987350, 1180043662, 1180069755 };
 std::vector<double> TSeqDep2 { 1178612270, 1178671357, 1179317171, 1179342922, 1179448830, 1179450402, 1179478475, 1179501801, 1179502951, 1179528921, 1179550506, 1179598045, 1179610476, 1179729725, 1179761228, 1179779152, 1179793471, 1179796369, 1179817032, 1179848189, 1179851753, 1179863962, 1179926541, 1179937736, 1179973158, 1179989775, 1180059958, 1188573325 };
 
 Case Real:
 
 std::vector<std::string> Fish1Seq_ {"Battle_Ck", "SR_JellysFerry", "SR_BendBridge", "SR_ChinaRapids", "SR_RedBluff", "SR_AbvThomes", "SR_BlwDryCk", "SR_BlwGCID", "SR_BlwIrvineFinch", "SR_AbvChicoCk", "SR_OrdBend", "SR_ButteCityBr", "SR_AbvColusaBr", "SR_MeridianBr", "SR_I-80/50Br", "SR_Freeport", "SR_SutterSl", "SR_SteamboatSl", "SR_RioVista", "Decker_IsS", "Chipps Island", "Benicia Bridge", "Carquinez Bridge", "Richmond Bridge", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line"};
 std::vector<std::string> Fish2Seq_ {"Battle_Ck", "SR_JellysFerry", "SR_BendBridge", "SR_ChinaRapids", "SR_RedBluff", "SR_BlwRBDD", "SR_AbvThomes", "SR_BlwDryCk", "SR_BlwGCID", "SR_BlwIrvineFinch", "SR_AbvChicoCk", "SR_OrdBend", "SR_ButteCityBr", "SR_AbvColusaBr", "SR_MeridianBr", "SR_I-80/50Br", "SR_Freeport", "SR_SutterSl", "SR_DCC", "Georg_SloughN", "SR_Ryde", "SR_Mouth", "SR_RioVista", "Decker_IsS", "Chipps Island", "Benicia Bridge", "Carquinez Bridge", "Richmond Bridge", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line"};
 std::vector<double> TSeqArr1 {1175915204, 1175921585, 1175933299, 1175959176, 1175998345, 1176023269, 1176079332, 1176266243, 1176282455, 1176284222, 1176509473, 1176630345, 1176843110, 1176855963, 1177006994, 1177024681, 1177064420, 1177172633, 1177174837, 1177183025, 1177262068, 1177337785, 1177344211, 1177423012, 1177433434, 1177433891, 1177434040, 1177434059, 1177434190, 1177434209};
 std::vector<double> TSeqDep1 {1175915632, 1175921661, 1175933602, 1175985034, 1176001695, 1176024023, 1176079432, 1176267247, 1176283789, 1176285668, 1176509998, 1176630691, 1176843147, 1176888391, 1177007445, 1177027080, 1177067169, 1177172633, 1177175264, 1177253292, 1177265600, 1177339378, 1177344596, 1177423867, 1177433845, 1177434014, 1177434040, 1177434164, 1177434190, 1177434675};
 std::vector<double> TSeqArr2 {1175916876, 1175923207, 1175941824, 1176032305, 1177176862, 1177179934, 1177221081, 1177225189, 1177305329, 1177313338, 1177314603, 1177355620, 1177409898, 1177478951, 1177495682, 1177641978, 1177668786, 1177694464, 1177708285, 1177711895, 1177715491, 1177748398, 1177754467, 1177764145, 1177847028, 1177887764, 1177922209, 1177984844, 1178016217, 1178018175, 1178018175, 1178018302, 1178018302, 1178018721, 1178018722, 1178018769, 1178018770, 1178018833, 1178018833, 1178018876, 1178018877, 1178019053, 1178019054, 1178019261, 1178019430, 1178019468, 1178019561, 1178019595, 1178025059, 1178041019, 1178041024, 1178041089, 1178041095, 1178041146, 1178041151, 1178041227, 1178041232, 1178041728, 1178041733, 1178041830, 1178041835, 1178041875, 1178041881, 1178041934, 1178041939, 1178041968, 1178041974, 1178042002, 1178042007, 1178042017, 1178042043, 1178042071, 1178042078, 1178042141, 1178042146, 1178042176, 1178042180, 1178042191, 1178042215, 1178042228, 1178042252, 1178042254, 1178042259, 1178042301, 1178042308, 1178042316, 1178042320, 1178042351, 1178042365, 1178042375, 1178042399, 1178042434, 1178042435, 1178042448, 1178042473, 1178042474, 1178042481, 1178042484, 1178042500, 1178042520, 1178042527, 1178042535, 1178042560, 1178042649, 1178042656, 1178042704, 1178042802, 1178042813, 1178042838, 1178042845, 1178042874, 1178042884, 1178042913, 1178042919, 1178042958, 1178042965};
 std::vector<double> TSeqDep2 {1175917353, 1175923399, 1175942117, 1176129237, 1177177951, 1177180066, 1177223070, 1177225427, 1177305978, 1177314277, 1177315316, 1177356506, 1177410255, 1177479162, 1177496033, 1177643071, 1177671186, 1177694642, 1177710065, 1177711989, 1177716197, 1177752157, 1177755722, 1177833576, 1177849988, 1177912784, 1177923260, 1178005006, 1178018134, 1178018175, 1178018232, 1178018302, 1178018659, 1178018721, 1178018725, 1178018769, 1178018773, 1178018833, 1178018837, 1178018876, 1178019002, 1178019053, 1178019206, 1178019430, 1178019430, 1178019557, 1178019561, 1178024389, 1178040957, 1178041019, 1178041033, 1178041089, 1178041103, 1178041146, 1178041160, 1178041227, 1178041670, 1178041728, 1178041804, 1178041830, 1178041837, 1178041875, 1178041883, 1178041934, 1178041948, 1178041968, 1178041975, 1178042002, 1178042009, 1178042036, 1178042051, 1178042071, 1178042113, 1178042141, 1178042148, 1178042176, 1178042183, 1178042213, 1178042220, 1178042228, 1178042252, 1178042254, 1178042293, 1178042301, 1178042308, 1178042316, 1178042340, 1178042360, 1178042367, 1178042396, 1178042426, 1178042434, 1178042441, 1178042465, 1178042473, 1178042474, 1178042481, 1178042489, 1178042513, 1178042520, 1178042527, 1178042535, 1178042619, 1178042649, 1178042697, 1178042799, 1178042802, 1178042835, 1178042838, 1178042874, 1178042874, 1178042910, 1178042913, 1178042933, 1178042958, 1178043840};
 
 Case Real Annoying (PLOTSET INDEX 11):
 
 std::vector<std::string> Fish1Seq_ {"Battle_Ck", "SR_JellysFerry", "SR_ChinaRapids", "SR_RedBluff", "SR_BlwRBDD", "SR_AbvThomes", "SR_BlwDryCk", "SR_BlwGCID", "SR_BlwIrvineFinch", "SR_AbvChicoCk", "SR_OrdBend", "SR_ButteCityBr", "SR_AbvColusaBr", "SR_MeridianBr", "SR_I-80/50Br", "SR_Freeport", "SR_SutterSl", "SR_DCC", "Georg_SloughN", "SR_Ryde", "SR_Mouth", "SR_RioVista", "Decker_IsS", "Chipps Island", "Benicia Bridge", "Carquinez Bridge", "Richmond Bridge", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "SR_RedBluff"};
 std::vector<std::string> Fish2Seq_ {"Battle_Ck", "SR_JellysFerry", "SR_BendBridge", "SR_ChinaRapids", "SR_RedBluff", "SR_BlwRBDD", "SR_AbvThomes", "SR_BlwDryCk", "SR_BlwGCID", "SR_BlwIrvineFinch", "SR_AbvChicoCk", "SR_OrdBend", "SR_ButteCityBr", "SR_AbvColusaBr", "SR_MeridianBr", "SR_I-80/50Br", "SR_Freeport", "SR_SutterSl", "SR_DCC", "Georg_SloughN", "SR_Ryde", "SR_Mouth", "SR_RioVista", "Decker_IsS", "Chipps Island", "Benicia Bridge", "Carquinez Bridge", "Richmond Bridge", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line"};
 std::vector<double> TSeqArr1 {1175915539, 1175939223, 1177318341, 1177336390, 1177339318, 1177451096, 1177459002, 1177565160, 1177574517, 1177576360, 1177650884, 1177684654, 1177751392, 1177774478, 1178165779, 1178215197, 1178237038, 1178251689, 1178257913, 1178273589, 1178291314, 1178295091, 1178304565, 1178362156, 1178374258, 1178381290, 1178450183, 1178463373, 1178464683, 1178464693, 1178464722, 1178469019, 1178526195, 1178526195, 1178526334, 1178526342, 1178526346, 1178526367, 1178526430, 1178526447, 1178526452, 1178526482, 1178526489, 1178526503, 1178526506, 1178526525, 1178526529, 1178526558, 1178526562, 1178526659, 1178526683, 1178526735, 1178526775, 1185345379};
 std::vector<double> TSeqDep1 {1175915896, 1175941019, 1177322115, 1177337341, 1177339360, 1177454352, 1177459057, 1177565913, 1177576116, 1177576993, 1177651781, 1177684878, 1177751530, 1177774695, 1178166650, 1178216801, 1178237248, 1178252812, 1178257982, 1178277654, 1178292798, 1178295923, 1178327830, 1178365883, 1178375121, 1178383563, 1178450991, 1178464597, 1178464683, 1178464693, 1178468938, 1178526182, 1178526195, 1178526311, 1178526334, 1178526342, 1178526346, 1178526427, 1178526441, 1178526449, 1178526477, 1178526482, 1178526489, 1178526503, 1178526517, 1178526525, 1178526551, 1178526558, 1178526635, 1178526659, 1178526711, 1178526735, 1178527812, 1189213379};
 std::vector<double> TSeqArr2 {1175916876, 1175923207, 1175941824, 1176032305, 1177176862, 1177179934, 1177221081, 1177225189, 1177305329, 1177313338, 1177314603, 1177355620, 1177409898, 1177478951, 1177495682, 1177641978, 1177668786, 1177694464, 1177708285, 1177711895, 1177715491, 1177748398, 1177754467, 1177764145, 1177847028, 1177887764, 1177922209, 1177984844, 1178016217, 1178018175, 1178018175, 1178018302, 1178018302, 1178018721, 1178018722, 1178018769, 1178018770, 1178018833, 1178018833, 1178018876, 1178018877, 1178019053, 1178019054, 1178019261, 1178019430, 1178019468, 1178019561, 1178019595, 1178025059, 1178041019, 1178041024, 1178041089, 1178041095, 1178041146, 1178041151, 1178041227, 1178041232, 1178041728, 1178041733, 1178041830, 1178041835, 1178041875, 1178041881, 1178041934, 1178041939, 1178041968, 1178041974, 1178042002, 1178042007, 1178042017, 1178042043, 1178042071, 1178042078, 1178042141, 1178042146, 1178042176, 1178042180, 1178042191, 1178042215, 1178042228, 1178042252, 1178042254, 1178042259, 1178042301, 1178042308, 1178042316, 1178042320, 1178042351, 1178042365, 1178042375, 1178042399, 1178042434, 1178042435, 1178042448, 1178042473, 1178042474, 1178042481, 1178042484, 1178042500, 1178042520, 1178042527, 1178042535, 1178042560, 1178042649, 1178042656, 1178042704, 1178042802, 1178042813, 1178042838, 1178042845, 1178042874, 1178042884, 1178042913, 1178042919, 1178042958, 1178042965};
 std::vector<double> TSeqDep2 {1175917353, 1175923399, 1175942117, 1176129237, 1177177951, 1177180066, 1177223070, 1177225427, 1177305978, 1177314277, 1177315316, 1177356506, 1177410255, 1177479162, 1177496033, 1177643071, 1177671186, 1177694642, 1177710065, 1177711989, 1177716197, 1177752157, 1177755722, 1177833576, 1177849988, 1177912784, 1177923260, 1178005006, 1178018134, 1178018175, 1178018232, 1178018302, 1178018659, 1178018721, 1178018725, 1178018769, 1178018773, 1178018833, 1178018837, 1178018876, 1178019002, 1178019053, 1178019206, 1178019430, 1178019430, 1178019557, 1178019561, 1178024389, 1178040957, 1178041019, 1178041033, 1178041089, 1178041103, 1178041146, 1178041160, 1178041227, 1178041670, 1178041728, 1178041804, 1178041830, 1178041837, 1178041875, 1178041883, 1178041934, 1178041948, 1178041968, 1178041975, 1178042002, 1178042009, 1178042036, 1178042051, 1178042071, 1178042113, 1178042141, 1178042148, 1178042176, 1178042183, 1178042213, 1178042220, 1178042228, 1178042252, 1178042254, 1178042293, 1178042301, 1178042308, 1178042316, 1178042340, 1178042360, 1178042367, 1178042396, 1178042426, 1178042434, 1178042441, 1178042465, 1178042473, 1178042474, 1178042481, 1178042489, 1178042513, 1178042520, 1178042527, 1178042535, 1178042619, 1178042649, 1178042697, 1178042799, 1178042802, 1178042835, 1178042838, 1178042874, 1178042874, 1178042910, 1178042913, 1178042933, 1178042958, 1178043840};
 
 Case Real Annoying 2 (PLOTSET INDEX 14):
 
 std::vector<std::string> Fish1Seq_ {"Battle_Ck", "SR_JellysFerry", "SR_ChinaRapids", "SR_RedBluff", "SR_BlwRBDD", "SR_AbvThomes", "SR_BlwDryCk", "SR_BlwGCID", "SR_BlwIrvineFinch", "SR_AbvChicoCk", "SR_OrdBend", "SR_ButteCityBr", "SR_AbvColusaBr", "SR_MeridianBr", "SR_I-80/50Br", "SR_Freeport", "SR_SutterSl", "SR_DCC", "Georg_SloughN", "SR_Ryde", "SR_Mouth", "SR_RioVista", "Decker_IsS", "Chipps Island", "Benicia Bridge", "Carquinez Bridge", "Richmond Bridge", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "SR_RedBluff"};
 std::vector<std::string> Fish2Seq_ {"Battle_Ck", "SR_JellysFerry", "SR_BendBridge", "SR_ChinaRapids", "SR_RedBluff", "SR_BlwRBDD", "SR_BlwMillCk", "SR_AbvThomes", "SR_BlwDryCk", "SR_BlwGCID", "AR_Mouth", "SR_BlwIrvineFinch", "SR_AbvChicoCk", "SR_OrdBend", "SR_ButteCityBr", "SR_AbvColusaBr", "SR_MeridianBr", "SR_I-80/50Br", "SR_Freeport", "SR_SutterSl", "SR_DCC", "Georg_SloughN", "Georg_SloughS", "Mok_GeorgianaS", "Mok_Will_Berm_Mar", "SJ_PortStockton", "SJ_Antioch", "Chipps Island", "Benicia Bridge", "Carquinez Bridge", "SFPiers", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Golden Gate West Line", "Golden Gate East Line", "Carquinez Bridge", "Benicia Bridge", "Chipps Island", "SJ_Antioch", "Antioch_Br", "SJ_CurtisLanding", "Antioch_Br", "SJ_CurtisLanding", "Antioch_Br", "SJ_CurtisLanding", "Antioch_Br", "SJ_CurtisLanding", "Antioch_Br", "SJ_CurtisLanding", "Antioch_Br", "SJ_CurtisLanding", "Antioch_Br", "SJ_CurtisLanding", "Antioch_Br", "SJ_CurtisLanding", "Antioch_Br", "SJ_CurtisLanding", "Antioch_Br", "SJ_CurtisLanding", "Antioch_Br", "SJ_CurtisLanding", "Antioch_Br", "FranksTractW", "FranksTractE", "SJ_PrisonersPt", "Mok_Will_Berm_Mar", "Mok_GeorgianaS", "Mok_Will_Berm_Mar", "FranksTractE", "SJ_PrisonersPt", "FranksTractE", "SJ_PrisonersPt", "FranksTractE", "SJ_PrisonersPt", "FranksTractE", "SJ_PrisonersPt", "FranksTractE", "SJ_PrisonersPt", "FranksTractE", "Mok_Will_Berm_Mar", "Mok_GeorgianaS", "Georg_SloughS", "Georg_SloughN", "SR_DCC", "SR_SutterSl", "SR_Freeport", "SR_I-80/50Br", "SR_AbvFeather", "SR_KnightsBr", "SR_MeridianBr", "SR_AbvColusaBr", "SR_ButteCityBr", "SR_OrdBend", "SR_BlwIrvineFinch", "SR_BlwGCID", "SR_BlwDryCk", "SR_AbvThomes", "SR_RedBluff", "SR_ChinaRapids", "SR_BendBridge", "SR_JellysFerry", "Battle_Ck"};
 std::vector<double> TSeqArr1 {1175915539, 1175939223, 1177318341, 1177336390, 1177339318, 1177451096, 1177459002, 1177565160, 1177574517, 1177576360, 1177650884, 1177684654, 1177751392, 1177774478, 1178165779, 1178215197, 1178237038, 1178251689, 1178257913, 1178273589, 1178291314, 1178295091, 1178304565, 1178362156, 1178374258, 1178381290, 1178450183, 1178463373, 1178464683, 1178464693, 1178464722, 1178469019, 1178526195, 1178526195, 1178526334, 1178526342, 1178526346, 1178526367, 1178526430, 1178526447, 1178526452, 1178526482, 1178526489, 1178526503, 1178526506, 1178526525, 1178526529, 1178526558, 1178526562, 1178526659, 1178526683, 1178526735, 1178526775, 1185345379};
 std::vector<double> TSeqDep1 {1175915896, 1175941019, 1177322115, 1177337341, 1177339360, 1177454352, 1177459057, 1177565913, 1177576116, 1177576993, 1177651781, 1177684878, 1177751530, 1177774695, 1178166650, 1178216801, 1178237248, 1178252812, 1178257982, 1178277654, 1178292798, 1178295923, 1178327830, 1178365883, 1178375121, 1178383563, 1178450991, 1178464597, 1178464683, 1178464693, 1178468938, 1178526182, 1178526195, 1178526311, 1178526334, 1178526342, 1178526346, 1178526427, 1178526441, 1178526449, 1178526477, 1178526482, 1178526489, 1178526503, 1178526517, 1178526525, 1178526551, 1178526558, 1178526635, 1178526659, 1178526711, 1178526735, 1178527812, 1189213379};
 std::vector<double> TSeqArr2 {1175938966, 1175983510, 1176023875, 1176034493, 1176092008, 1176095438, 1176201270, 1176249199, 1176252654, 1176367215, 1176438794, 1176535049, 1176536951, 1177501572, 1177560022, 1177674398, 1177693783, 1177902228, 1177930732, 1177958594, 1177986890, 1177990380, 1178017461, 1178020289, 1178023454, 1178027876, 1178108670, 1178115046, 1178184834, 1178193411, 1178307207, 1178315520, 1178316742, 1178316747, 1178316806, 1178316811, 1178316851, 1178316857, 1178316909, 1178316950, 1178316989, 1178317075, 1178317141, 1178317267, 1178317268, 1185683575, 1185683578, 1185683720, 1185683723, 1185714192, 1185714369, 1185715515, 1186340343, 1186353696, 1186414152, 1186435348, 1186442822, 1186452914, 1186452918, 1186452920, 1186452953, 1186452955, 1186452960, 1186452973, 1186452990, 1186452992, 1186453031, 1186453033, 1186453079, 1186453081, 1186453122, 1186453134, 1186453226, 1186453238, 1186453297, 1186453352, 1186453405, 1186453460, 1186453621, 1186461498, 1186488711, 1186496387, 1186523292, 1186528316, 1186538559, 1186553228, 1186558130, 1186600673, 1186600731, 1186601097, 1186601904, 1186616841, 1186618070, 1186634957, 1186762135, 1186771559, 1186787531, 1186793820, 1186797287, 1186833971, 1186836814, 1186859636, 1186902350, 1186946879, 1187046705, 1187111160, 1187374779, 1187444187, 1187656371, 1187881739, 1188108234, 1188201310, 1188557832, 1188603802, 1188947480, 1189089174, 1189367481, 1189715415, 1189915512};
 std::vector<double> TSeqDep2 {1175939970, 1175983617, 1176023952, 1176060392, 1176093213, 1176095542, 1176201270, 1176250411, 1176252702, 1176368132, 1176438794, 1176536036, 1176537508, 1177503304, 1177561203, 1177674454, 1177694601, 1177903186, 1177932333, 1177958594, 1177988143, 1177991140, 1178019178, 1178021383, 1178025339, 1178028639, 1178109740, 1178118878, 1178185647, 1178193535, 1178307921, 1178316683, 1178316742, 1178316757, 1178316806, 1178316821, 1178316851, 1178316864, 1178316925, 1178316950, 1178316989, 1178317075, 1178317252, 1178317267, 1185683567, 1185683575, 1185683712, 1185683720, 1185714019, 1185714192, 1185714900, 1185718834, 1186342295, 1186383962, 1186416343, 1186437074, 1186452901, 1186452914, 1186452918, 1186452920, 1186452953, 1186452955, 1186452960, 1186452973, 1186452990, 1186453008, 1186453033, 1186453045, 1186453079, 1186453081, 1186453122, 1186453197, 1186453226, 1186453287, 1186453344, 1186453399, 1186453405, 1186453499, 1186455140, 1186466311, 1186495902, 1186496533, 1186526437, 1186535213, 1186543663, 1186554414, 1186600479, 1186600673, 1186600941, 1186601097, 1186611416, 1186617946, 1186631556, 1186655045, 1186770979, 1186771559, 1186789625, 1186794881, 1186800742, 1186835038, 1186841259, 1186859896, 1186904630, 1186947097, 1187048247, 1187112673, 1187376396, 1187444353, 1187657284, 1187886989, 1188113572, 1188204264, 1188558138, 1188608373, 1188991746, 1189092803, 1189507246, 1189718001, 1189917353};
 
 Case Real Test 3:
 
 std::vector<std::string> Fish1Seq_ {"Battle_Ck", "SR_JellysFerry", "SR_BendBridge", "SR_ChinaRapids", "SR_RedBluff", "SR_AbvThomes", "SR_BlwDryCk", "SR_BlwGCID", "SR_BlwIrvineFinch", "SR_AbvChicoCk", "SR_OrdBend", "SR_ButteCityBr", "SR_AbvColusaBr", "SR_MeridianBr", "SR_I-80/50Br", "SR_Freeport", "SR_SutterSl", "SR_DCC", "Georg_SloughN", "SR_Ryde", "SR_Mouth", "SR_RioVista", "Decker_IsS", "SJ_Antioch", "Chipps Island", "Benicia Bridge", "Carquinez Bridge", "Richmond Bridge", "SFPiers", "Golden Gate and Ocean", "SR_AbvChicoCk", "Golden Gate and Ocean", "Carquinez Bridge", "Benicia Bridge", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "SR_RioVista", "SR_Mouth", "SR_Ryde", "Georg_SloughN", "SR_DCC", "SR_SutterSl", "SR_Freeport", "SR_I-80/50Br", "SR_AbvFeather", "SR_KnightsBr", "SR_MeridianBr", "SR_AbvColusaBr", "SR_ButteCityBr", "SR_OrdBend", "SR_BlwIrvineFinch", "SR_BlwGCID", "SR_BlwDryCk", "SR_AbvThomes", "SR_BlwMillCk", "SR_RedBluff", "SR_ChinaRapids", "SR_BendBridge", "SR_JellysFerry", "Battle_Ck"};
 std::vector<std::string> Fish2Seq_ {"Battle_Ck", "SR_JellysFerry", "SR_BendBridge", "SR_ChinaRapids", "SR_RedBluff", "SR_BlwRBDD", "SR_AbvThomes", "SR_BlwGCID", "SR_BlwIrvineFinch", "SR_AbvChicoCk", "SR_OrdBend", "SR_ButteCityBr", "SR_AbvColusaBr", "SR_MeridianBr", "SR_I-80/50Br", "SR_Freeport", "SR_SutterSl", "SR_DCC", "Georg_SloughN", "Georg_SloughS", "Mok_GeorgianaS", "Mok_Will_Berm_Mar", "SJ_PortStockton", "SJ_Antioch", "Chipps Island", "Benicia Bridge", "Carquinez Bridge", "Benicia Bridge", "Chipps Island", "Benicia Bridge", "Carquinez Bridge", "Richmond Bridge", "SFPiers", "Golden Gate and Ocean", "Carquinez Bridge", "Benicia Bridge", "Chipps Island", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "SR_RioVista", "SR_Mouth", "SR_Ryde", "Georg_SloughN", "SR_DCC", "SR_SutterSl", "SR_Freeport", "SR_I-80/50Br", "SR_AbvFeather", "SR_KnightsBr", "SR_MeridianBr", "SR_AbvColusaBr", "SR_ButteCityBr", "SR_OrdBend", "SR_BlwIrvineFinch", "SR_BlwGCID", "SR_BlwDryCk", "SR_AbvThomes", "SR_BlwMillCk", "SR_BlwRBDD", "SR_RedBluff", "SR_ChinaRapids", "SR_BendBridge", "SR_JellysFerry", "SR_AbvBattleCk", "Battle_Ck", "SR_AbvBattleCk"};
 std::vector<double> TSeqArr1 {1177665624, 1177733195, 1177849193, 1177903830, 1177914670, 1177979042, 1177982255, 1178008446, 1178019593, 1178035703, 1178089376, 1178419666, 1178508036, 1178532004, 1178679657, 1178710082, 1178743771, 1178752653, 1178780792, 1178785475, 1178809618, 1178815350, 1178824215, 1178858755, 1178878981, 1178917488, 1178965371, 1179037015, 1179054759, 1179077681, 1181356475, 1186942035, 1187392614, 1187505842, 1187605195, 1187610929, 1187610943, 1187610966, 1187611047, 1187611073, 1187611130, 1187611156, 1187614870, 1187619618, 1187619668, 1187619688, 1187619951, 1187621382, 1187621423, 1187621450, 1187621489, 1187621505, 1187621539, 1187621579, 1187621656, 1187621695, 1187621723, 1187621742, 1187621775, 1187621812, 1187621845, 1187622056, 1187622113, 1187638333, 1187694602, 1187783038, 1187794047, 1187796721, 1187825021, 1187887914, 1187949798, 1188061975, 1188111171, 1188372235, 1188433528, 1188669792, 1188832265, 1189083145, 1189153079, 1189385189, 1189400131, 1189423188, 1189618566, 1189666029, 1190580429, 1190946346, 1191494046};
 std::vector<double> TSeqDep1 {1177666321, 1177734043, 1177849364, 1177907125, 1177915430, 1177980160, 1177982255, 1178009072, 1178035270, 1178037442, 1178090712, 1178419954, 1178508349, 1178532391, 1178681598, 1178722121, 1178743771, 1178778211, 1178780953, 1178786424, 1178811419, 1178816018, 1178827687, 1178864123, 1178905557, 1178955661, 1178985487, 1179037351, 1179071780, 1179163835, 1181359841, 1186948979, 1187434884, 1187509582, 1187610409, 1187610929, 1187610943, 1187610966, 1187611047, 1187611073, 1187611130, 1187614864, 1187619531, 1187619618, 1187619668, 1187619901, 1187621348, 1187621382, 1187621423, 1187621450, 1187621489, 1187621505, 1187621539, 1187621646, 1187621656, 1187621695, 1187621723, 1187621762, 1187621775, 1187621812, 1187622050, 1187622056, 1187622578, 1187690850, 1187699486, 1187785410, 1187794437, 1187804070, 1187825433, 1187892597, 1187952156, 1188063166, 1188113938, 1188375719, 1188434522, 1188670747, 1188836297, 1189098639, 1189155642, 1189385487, 1189402544, 1189423519, 1189625008, 1189682182, 1190585001, 1191023561, 1191495203};
 std::vector<double> TSeqArr2 {1176808497, 1176856328, 1176872098, 1176887769, 1176942802, 1176945824, 1177471266, 1177580415, 1177641943, 1177643897, 1177700543, 1177741422, 1177779929, 1177794831, 1177911848, 1177945040, 1177969495, 1177985344, 1177990274, 1178018649, 1178022112, 1178025439, 1178029346, 1178110631, 1178117365, 1178283037, 1178294740, 1178312502, 1178438389, 1178547411, 1178554521, 1178635905, 1178650148, 1178672898, 1187502635, 1187547548, 1187612629, 1187625376, 1187630434, 1187630622, 1187630714, 1187684588, 1187696521, 1187721482, 1187729060, 1187731675, 1187780215, 1187818821, 1187877517, 1187984023, 1188020079, 1188245157, 1188304312, 1188528991, 1188695663, 1188879811, 1188964666, 1189650525, 1189788011, 1189925420, 1190903560, 1191395794, 1191749097, 1192425175, 1192846627, 1192939088, 1196999149, 1197010723};
 std::vector<double> TSeqDep2 {1176832231, 1176856404, 1176872609, 1176888809, 1176944284, 1176946235, 1177473557, 1177582285, 1177643546, 1177645004, 1177717761, 1177741664, 1177779992, 1177795168, 1177912638, 1177946756, 1177969604, 1177987349, 1177990917, 1178020757, 1178023005, 1178026981, 1178031139, 1178111622, 1178153474, 1178284923, 1178301638, 1178393439, 1178453267, 1178548743, 1178555819, 1178635986, 1178653697, 1187288383, 1187507235, 1187579333, 1187613139, 1187630356, 1187630506, 1187630622, 1187671306, 1187690024, 1187698161, 1187722992, 1187730421, 1187761525, 1187780494, 1187821355, 1187878499, 1187985462, 1188021737, 1188246489, 1188304665, 1188530909, 1188697020, 1188884791, 1188968777, 1189650971, 1189792215, 1189937121, 1191288762, 1191398383, 1191752768, 1192449727, 1192847121, 1196821916, 1196999808, 1197011562};
 
 Problem case from sig testing:
 
 std::vector<std::string> Fish1Seq_ { "Battle_Ck", "SR_JellysFerry", "SR_BendBridge", "SR_ChinaRapids", "SR_RedBluff", "SR_BlwRBDD", "SR_AbvThomes", "SR_BlwGCID", "SR_BlwIrvineFinch", "SR_AbvChicoCk", "SR_OrdBend", "SR_ButteCityBr", "SR_AbvColusaBr", "SR_MeridianBr", "SR_I-80/50Br", "SR_Freeport", "SR_DCC", "Georg_SloughN", "SR_Ryde", "SR_Mouth", "SR_RioVista", "HorseshoeBend", "Decker_IsS", "SJ_Antioch", "Chipps Island", "Benicia Bridge", "Carquinez Bridge", "Benicia Bridge", "Carquinez Bridge", "Richmond Bridge", "Golden Gate and Ocean", "Carquinez Bridge", "Benicia Bridge", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "SR_RioVista", "SR_Mouth", "SR_Ryde", "Georg_SloughN", "SR_DCC", "SR_SutterSl", "SR_Freeport", "SR_I-80/50Br", "SR_AbvFeather", "SR_KnightsBr", "SR_MeridianBr", "SR_AbvColusaBr", "SR_ButteCityBr", "SR_OrdBend", "SR_BlwIrvineFinch", "SR_BlwGCID", "SR_BlwDryCk", "SR_AbvThomes", "SR_BlwMillCk", "SR_BlwRBDD", "SR_RedBluff", "SR_ChinaRapids", "SR_BendBridge", "SR_JellysFerry", "Battle_Ck" };
 std::vector<std::string> Fish2Seq_ { "Battle_Ck", "SR_JellysFerry", "SR_BendBridge", "SR_ChinaRapids", "SR_RedBluff", "SR_BlwRBDD", "SR_AbvThomes", "SR_BlwGCID", "SR_BlwIrvineFinch", "SR_AbvChicoCk", "SR_OrdBend", "SR_ButteCityBr", "SR_AbvColusaBr", "SR_MeridianBr", "AR_Mouth", "SR_BlwIrvineFinch", "SR_AbvChicoCk", "SR_BlwIrvineFinch", "SR_AbvChicoCk", "SR_BlwIrvineFinch", "SR_AbvChicoCk", "SR_OrdBend", "SR_ButteCityBr", "SR_AbvColusaBr", "SR_MeridianBr", "SR_I-80/50Br", "SR_Freeport", "SR_SutterSl", "SR_DCC", "Georg_SloughN", "SR_Ryde", "Georg_SloughN", "SR_Ryde", "SR_Mouth", "SR_RioVista", "Decker_IsS", "Chipps Island", "Benicia Bridge", "Carquinez Bridge", "PardiseCay", "Richmond Bridge", "Golden Gate and Ocean", "Carquinez Bridge", "Benicia Bridge", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "Decker_IsS", "HorseshoeBend", "SR_RioVista", "SR_Mouth", "SR_Ryde", "Georg_SloughN", "SR_DCC", "SR_SutterSl", "SR_Freeport", "SR_I-80/50Br", "SR_AbvFeather", "SR_KnightsBr", "SR_MeridianBr", "SR_AbvColusaBr", "SR_ButteCityBr", "SR_OrdBend", "SR_BlwIrvineFinch", "SR_BlwGCID", "SR_BlwDryCk", "SR_AbvThomes", "SR_RedBluff", "SR_ChinaRapids", "SR_BendBridge", "SR_JellysFerry", "SR_AbvBattleCk", "Battle_Ck", "SR_AbvBattleCk" };
 std::vector<double> TSeqArr1 { 0, 44237, 142192, 231066, 289743, 292123, 314276, 402348, 433590, 433625, 490795, 820635, 934054, 952788, 1099314, 1133089, 1172172, 1175370, 1179995, 1210231, 1212936, 1226656, 1235454, 1271329, 1282440, 1310225, 1344955, 1360904, 1390639, 1452556, 1479922, 1696701, 1709177, 1835056, 1835540, 1835597, 1835636, 1835717, 1835744, 1836073, 1836107, 1876780, 1877011, 1877017, 1877036, 1880801, 1881098, 1881155, 1885894, 1885975, 1887391, 1900673, 1956942, 2045378, 2056387, 2059738, 2085134, 2123740, 2182182, 2287924, 2352028, 2611983, 2671423, 2907687, 3134060, 3360555, 3463787, 3821466, 3958952, 3984167, 4962307, 5233268, 5318555, 6220431, 6641883, 7112862 };
 std::vector<double> TSeqDep1 { 1245508, 1245559, 1245455, 1245444, 1245250, 1245263, 1245492, 1245511, 1245119, 1245541, 1245523, 1245566, 1245561, 1245138, 1245283, 1245240, 1245362, 1245476, 1245198, 1245166, 1245123, 1245125, 1245173, 1245338, 1245580, 1245266, 1245513, 1245564, 1245569, 1245349, 1245576, 1245334, 1245108, 1245201, 1245555, 1245545, 1245226, 1245532, 1245382, 1245547, 1245447, 1245295, 1245549, 1245415, 1245339, 1245273, 1245210, 1245184, 1245421, 1245275, 1245305, 1245461, 1245529, 1245536, 1245288, 1245433, 1245315, 1245302, 1245206, 1245152, 1245162, 1245280, 1245509, 1245527, 1245488, 1245153, 1245110, 1245410, 1245318, 1245171, 1245314, 1245278, 1245183, 1245553, 1245211, 1245431 };
 std::vector<double> TSeqArr2 { 0, 24113, 35831, 124799, 155836, 159506, 433287, 1067254, 1128053, 1137800, 1167018, 1215068, 1298978, 1322702, 1476610, 1580598, 1582497, 1593825, 1595710, 1596731, 1598407, 2563163, 2587965, 2658710, 2678443, 2886369, 2919579, 2961864, 2980219, 2983470, 3001943, 3008799, 3011431, 3026021, 3031876, 3040741, 3114920, 3211160, 3257471, 3301072, 3303826, 3387065, 4094728, 4170286, 4297684, 4297909, 4298031, 4298057, 4298114, 4298137, 4298218, 4298237, 4298500, 4298523, 4298793, 4299021, 4299071, 4300487, 4300537, 4301042, 4301052, 4306071, 4306121, 4306137, 4306165, 4306664, 4306745, 4306772, 4306829, 4306869, 4306883, 4312189, 4312459, 4312687, 4312701, 4317759, 4317792, 4322480, 4322508, 4322545, 4322559, 4327247, 4327275, 4327295, 4367921, 4367999, 4368056, 4368134, 4368215, 4368221, 4368271, 4368363, 4368582, 4368601, 4409227, 4413907, 4427189, 4486203, 4574639, 4582217, 4584383, 4612683, 4655416, 4717300, 4817890, 4853698, 5114762, 5176055, 5400734, 5626144, 5878242, 5973611, 6658313, 6704423, 7048101, 7405257, 8307175, 8673092, 8765553, 12825614, 12837188 };
 std::vector<double> TSeqDep2 { 4508688, 4609665, 4469098, 4476837, 4600696, 4628089, 4549899, 4494577, 4449883, 4451508, 4482050, 4503503, 4476183, 4485139, 4458061, 4651287, 4683042, 4617170, 4610973, 4520652, 4635257, 4680916, 4597787, 4464366, 4469859, 4447118, 4582100, 4514538, 4516316, 4474992, 4587348, 4673264, 4545703, 4660496, 4545679, 4577044, 4666402, 4641865, 4455446, 4514776, 4513697, 4597814, 4603114, 4620692, 4505630, 4456501, 4470088, 4524826, 4599706, 4505738, 4507327, 4586187, 4682801, 4606361, 4596323, 4678754, 4612570, 4647597, 4506280, 4558905, 4627435, 4483459, 4512069, 4583281, 4596447, 4510587, 4668425, 4570666, 4524867, 4494228, 4553099, 4679060, 4684835, 4614835, 4665708, 4676661, 4654303, 4518243, 4471550, 4449062, 4502312, 4441145, 4650577, 4596007, 4558856, 4684637, 4651037, 4685788, 4639663, 4592869, 4605396, 4561629, 4468248, 4549679, 4612077, 4498294, 4576072, 4630544, 4534753, 4609376, 4463158, 4610258, 4570195, 4501224, 4554889, 4580775, 4619510, 4550246, 4653344, 4509316, 4548034, 4629765, 4595737, 4538468, 4488757, 4470938, 4542241, 4446909, 4543647, 4612026, 4498536 };
 
 Chinook test one:
 
 std::vector<std::string> Fish1Seq_ { "ColemanRel", "Battle_Ck", "SR_JellysFerry", "Battle_Ck", "SR_JellysFerry", "Battle_Ck", "SR_SturgeonHole", "Battle_Ck", "SR_SturgeonHole", "Battle_Ck", "SR_SturgeonHole", "Battle_Ck", "SR_SturgeonHole", "Battle_Ck", "SR_SturgeonHole", "Battle_Ck", "SR_SturgeonHole", "Battle_Ck", "SR_SturgeonHole", "Battle_Ck", "SR_SturgeonHole", "Battle_Ck", "SR_SturgeonHole", "Battle_Ck", "SR_SturgeonHole", "Battle_Ck", "SR_SturgeonHole", "Battle_Ck", "SR_BoulderHole", "Battle_Ck", "SR_BendBridge", "Battle_Ck", "SR_BendBridge", "Battle_Ck", "SR_BendBridge", "Battle_Ck", "SR_BendBridge", "Battle_Ck", "SR_ChinaRapids", "SR_RedBluff", "SR_BlwRBDD", "SR_SaltCk", "SR_AbvAntelopeCk", "SR_BlwAntelopeCk", "SR_AbvThomes", "SR_AbvToomes", "Woodson Temp", "SR_AbvGCID", "SR_BlwGCID", "SR_BlwIrvineFinch", "SR_AbvOrd", "SR_BlwOrd", "SR_ButteCityBr", "SR_BlwButte", "SR_AbvColusaBr", "SR_AbvTisdale", "SR_BlwChinaBend", "AlcatrazSW", "SR_KnightsLanding", "SR_AbvFeather", "FR_Verona", "SR_I-80/50Br", "SR_GB", "SR_Freeport", "SR_GB", "SR_KK", "SR_BlwSteam", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_BlwGeorgiana", "SR_KK", "SR_RV", "SR_Mouth", "SR_RV", "SR_RioVista", "SR_RV", "SR_RioVista", "SR_RV", "SR_RioVista", "SR_RV", "Decker_IsN", "ThreeMile", "SJ_Antioch", "Chipps Island", "SP_Buoy", "SP_Control", "Richmond Bridge", "Bay Bridge", "Pt_Reyes" };
 std::vector<std::string> Fish2Seq_ { "ColemanRel", "Battle_Ck", "Massacre Flat", "Battle_Ck", "SR_SturgeonHole", "Battle_Ck", "SR_BendBridge", "Battle_Ck", "SR_ChinaRapids", "SR_RedBluff", "SR_SaltCk", "SR_AbvThomes", "Woodson Temp", "SR_AbvGCID", "SR_BlwGCID", "SR_BlwIrvineFinch", "SR_AbvOrd", "SR_BlwOrd", "SR_AbvOrd", "SR_BlwOrd", "SR_AbvOrd", "SR_BlwOrd", "SR_AbvOrd", "SR_BlwOrd", "SR_AbvOrd", "SR_BlwOrd", "SR_AbvOrd", "SR_BlwOrd", "SR_ButteCityBr", "SR_BlwButte", "SR_AbvColusaBr", "SR_MeridianBr", "SR_AbvTisdale", "SR_BlwChinaBend", "AlcatrazSW", "SR_KnightsLanding", "SR_AbvFeather", "FR_Verona", "SR_I-80/50Br", "SR_GB", "SR_Freeport", "SR_GB", "SR_KK", "SR_BlwSteam", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "Georg_SloughN", "SR_KK", "Georg_SloughN", "SR_KK", "SR_DCC", "Georg_SloughN", "SR_BlwGeorgiana", "SR_KK", "SR_BlwGeorgiana", "SR_KK", "SR_RV", "SR_Mouth", "SR_RV", "SR_SteamboatSl", "SR_RioVista", "SR_RV", "SR_RioVista", "SR_RV", "Decker_IsN", "Decker_IsS", "SJ_Antioch", "Chipps Island", "SJ_Antioch", "SJ_JerseyPoint", "Mok_Will_Berm_Mar", "Mok_GeorgianaS", "Georg_SloughS", "Mok_GeorgianaS", "Georg_SloughS", "Georg_SloughN", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_DCC", "SR_KK", "SR_BlwSteam", "CarSt", "SR_SteamboatSl", "SR_SutterSl", "Miner Slough", "SR_RV", "SR_RioVista", "SR_RV", "SR_RioVista", "SR_RV", "Decker_IsN", "SR_RV", "Decker_IsN", "Decker_IsS", "Chipps Island", "Benicia Bridge", "Carquinez Bridge", "SP_Control", "SP_Array", "SP_Control", "SP_Array", "SP_Control", "SP_Array", "Richmond Bridge", "SP_Array", "SP_Control", "SP_Buoy", "Mare_Island", "SP_Control", "SP_Array", "SP_Flats_Array", "SP_Control", "SP_Buoy", "Mare_Island", "Vallejo Marina" };
 std::vector<double> TSeqArr1 { 1260974820, 1261035234, 1261184036, 1261184038, 1261184082, 1261184085, 1261190174, 1261190196, 1261190252, 1261190256, 1261190294, 1261190304, 1261190352, 1261190352, 1261190446, 1261190473, 1261190477, 1261190493, 1261190520, 1261190536, 1261190556, 1261190562, 1261190581, 1261190593, 1261190610, 1261190638, 1261190650, 1261190677, 1261191927, 1261192000, 1261198870, 1261198891, 1261198913, 1261198925, 1261198975, 1261199020, 1261199048, 1261199065, 1261209367, 1261217483, 1261225354, 1261228675, 1261309910, 1261311390, 1261374752, 1261381376, 1261387898, 1261642661, 1261646740, 1261671621, 1261870871, 1262023314, 1262229925, 1262549972, 1262584447, 1262662025, 1262710533, 1262745952, 1262750129, 1262770563, 1262776210, 1262867484, 1263585529, 1263605822, 1263609947, 1263646991, 1263659880, 1263692090, 1263692471, 1263692494, 1263692512, 1263692523, 1263692585, 1263692590, 1263692625, 1263692777, 1263692777, 1263692859, 1263692916, 1263693328, 1263694922, 1263697803, 1263779012, 1263782741, 1263785004, 1263792963, 1263794511, 1263803398, 1263803793, 1263918334, 1263937971, 1263959156, 1264030244, 1264259044, 1264308108, 1264532916, 1264534473, 1264539937, 1264557823, 1265309711 };
 std::vector<double> TSeqDep1 { 1260974820, 1261184004, 1261184036, 1261184038, 1261184082, 1261190173, 1261190196, 1261190222, 1261190252, 1261190256, 1261190294, 1261190304, 1261190352, 1261190439, 1261190446, 1261190473, 1261190477, 1261190514, 1261190520, 1261190536, 1261190556, 1261190562, 1261190581, 1261190593, 1261190610, 1261190638, 1261190650, 1261191913, 1261191961, 1261198865, 1261198870, 1261198891, 1261198913, 1261198973, 1261198975, 1261199020, 1261199048, 1261206855, 1261209655, 1261217829, 1261225354, 1261229116, 1261309910, 1261312156, 1261375536, 1261381428, 1261388782, 1261643397, 1261646933, 1261697227, 1261871140, 1262052352, 1262230831, 1262550277, 1262584595, 1262662025, 1262710624, 1262746174, 1262750129, 1262771090, 1262776287, 1262868075, 1263602741, 1263606352, 1263614463, 1263650678, 1263660810, 1263692455, 1263692471, 1263692494, 1263692512, 1263692523, 1263692585, 1263692590, 1263692744, 1263692777, 1263692777, 1263692859, 1263693117, 1263693328, 1263695151, 1263703992, 1263779519, 1263783498, 1263785085, 1263793239, 1263803174, 1263803639, 1263803958, 1263918334, 1263952487, 1263959324, 1264031220, 1264259044, 1264356251, 1264532916, 1264534523, 1264539937, 1264574214, 1268773232 };
 std::vector<double> TSeqArr2 { 1260974820, 1261088288, 1261465390, 1261466144, 1261467140, 1261472340, 1261475858, 1261476258, 1261486726, 1261493359, 1261531101, 1261549955, 1261562050, 1261635724, 1261640109, 1261651703, 1261929971, 1261939708, 1261939735, 1261939750, 1261939777, 1261939784, 1261939813, 1261939823, 1261939835, 1261939844, 1261939861, 1261968565, 1262041573, 1262215733, 1262300926, 1262422640, 1262450928, 1262502339, 1262534801, 1262541045, 1262709742, 1262714692, 1262781070, 1262964438, 1263048924, 1263075935, 1263121042, 1263134341, 1263215715, 1263255258, 1263255286, 1263255374, 1263255403, 1263255413, 1263255448, 1263255484, 1263255488, 1263255614, 1263255633, 1263255660, 1263302455, 1263302480, 1263302580, 1263302580, 1263302750, 1263302750, 1263302772, 1263302795, 1263302827, 1263302864, 1263302868, 1263302933, 1263302942, 1263302965, 1263302988, 1263303009, 1263303056, 1263303119, 1263303126, 1263303151, 1263303157, 1263303197, 1263303201, 1263303240, 1263303241, 1263303278, 1263303343, 1263303426, 1263303470, 1263303492, 1263303574, 1263303690, 1263303857, 1263303934, 1263303959, 1263304037, 1263304081, 1263304176, 1263304206, 1263304208, 1263304230, 1263304253, 1263304259, 1263304377, 1263304400, 1263304571, 1263304618, 1263304619, 1263304666, 1263305588, 1263305603, 1263306782, 1263306819, 1263407179, 1263407206, 1263407307, 1263407328, 1263407373, 1263407376, 1263407558, 1263407570, 1263407619, 1263407628, 1263407740, 1263407755, 1263407786, 1263407816, 1263407866, 1263407879, 1263408019, 1263408026, 1263408062, 1263408063, 1263408099, 1263408216, 1263408257, 1263408259, 1263408306, 1263409056, 1263409069, 1263409255, 1263409396, 1263409422, 1263409704, 1263409786, 1263420308, 1263945091, 1263948821, 1264048265, 1264051319, 1264052623, 1264075867, 1264733782, 1264734329, 1264759046, 1264760813, 1264781683, 1264821514, 1264975921, 1265001897, 1265062823, 1265192864, 1265216707, 1265230910, 1265238172, 1265258294, 1265326206, 1265469783, 1265472987, 1265472999, 1265473043, 1265473287, 1265473353, 1265473481, 1265473538, 1265473569, 1265473610, 1265473723, 1265493563, 1265507915, 1265514411, 1265648705, 1265681730, 1265755763, 1265756308, 1265756598, 1265756666, 1265760179, 1265803173, 1265831304, 1265838495, 1265847468, 1265937164, 1266010091, 1266048695, 1266110270, 1266111599, 1266152348, 1266159039, 1266185473, 1266187870, 1266197942, 1266210203, 1266213252, 1266218298, 1266264615, 1266285677, 1266287859, 1266297788, 1266325316, 1266345896, 1266439380, 1266465537 };
 std::vector<double> TSeqDep2 { 1260974820, 1261450629, 1261465390, 1261466311, 1261467278, 1261473839, 1261475954, 1261476258, 1261487078, 1261493359, 1261531288, 1261550093, 1261562423, 1261636803, 1261640286, 1261652566, 1261939704, 1261939708, 1261939735, 1261939750, 1261939777, 1261939803, 1261939813, 1261939823, 1261939835, 1261939844, 1261954239, 1261969136, 1262042833, 1262216065, 1262301048, 1262448038, 1262450928, 1262502493, 1262535339, 1262541439, 1262710309, 1262715603, 1262781265, 1263045220, 1263058183, 1263081097, 1263123635, 1263158593, 1263255240, 1263255258, 1263255331, 1263255374, 1263255403, 1263255413, 1263255448, 1263255484, 1263255488, 1263255614, 1263255633, 1263302435, 1263302455, 1263302558, 1263302580, 1263302676, 1263302750, 1263302771, 1263302772, 1263302824, 1263302827, 1263302864, 1263302922, 1263302933, 1263302964, 1263302965, 1263302988, 1263303009, 1263303056, 1263303119, 1263303126, 1263303151, 1263303157, 1263303197, 1263303201, 1263303240, 1263303241, 1263303336, 1263303389, 1263303464, 1263303470, 1263303529, 1263303684, 1263303690, 1263303914, 1263303934, 1263304034, 1263304066, 1263304127, 1263304176, 1263304206, 1263304208, 1263304230, 1263304253, 1263304328, 1263304377, 1263304520, 1263304571, 1263304618, 1263304619, 1263305562, 1263305588, 1263306761, 1263306782, 1263407130, 1263407179, 1263407295, 1263407307, 1263407328, 1263407373, 1263407532, 1263407558, 1263407595, 1263407619, 1263407717, 1263407740, 1263407782, 1263407786, 1263407856, 1263407866, 1263407983, 1263408019, 1263408026, 1263408062, 1263408089, 1263408208, 1263408216, 1263408257, 1263408259, 1263408629, 1263409056, 1263409069, 1263409255, 1263409396, 1263409485, 1263409704, 1263416882, 1263943010, 1263946822, 1263953342, 1264049568, 1264051891, 1264053601, 1264688639, 1264734285, 1264756409, 1264760562, 1264771566, 1264803678, 1264897898, 1264987697, 1265051793, 1265100530, 1265192999, 1265220672, 1265231438, 1265238227, 1265281789, 1265387771, 1265472586, 1265472987, 1265472999, 1265473270, 1265473342, 1265473473, 1265473481, 1265473538, 1265473569, 1265473610, 1265475054, 1265493563, 1265511332, 1265514642, 1265663463, 1265742777, 1265756255, 1265756365, 1265756598, 1265756666, 1265795989, 1265805070, 1265832735, 1265839162, 1265893110, 1265937861, 1266011719, 1266096863, 1266111406, 1266116871, 1266156373, 1266166573, 1266186751, 1266190255, 1266197942, 1266212187, 1266213941, 1266218803, 1266267917, 1266285964, 1266289503, 1266300674, 1266340979, 1266360409, 1266440142, 1266466374 };
 
 Blank for future test cases:
 
 std::vector<std::string> Fish1Seq_ {};
 std::vector<std::string> Fish2Seq_ {};
 std::vector<double> TSeqArr1 {};
 std::vector<double> TSeqDep1 {};
 std::vector<double> TSeqArr2 {};
 std::vector<double> TSeqDep2 {};
 */
