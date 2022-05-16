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
#include <stack>
#include <unordered_map>




double DiffM(std::vector<std::vector<double>> C1, std::vector<std::vector<double>> C2, std::vector<int> Const1, std::vector<int> Const2, std::vector<std::vector<double>> &DiffLUT){
    
    std::vector<double> DiffVec(C1.size(), NAN);
    
    double DiffMean;
    for(int k = 0; k<C1.size(); k++){
        if(!(DiffLUT[Const1[k]][Const2[k]] > 0)){
            DiffVec[k] = ((C1[k][0] - C2[k][0]) + (C1[k][1] - C2[k][1]));
            DiffLUT[Const1[k]][Const2[k]] = DiffVec[k];
        } else{
            DiffVec[k] = DiffLUT[Const1[k]][Const2[k]];
        }
    }
    
    double DiffSum = std::accumulate(DiffVec.begin(), DiffVec.end(), 0LL);
    DiffMean = DiffSum / (2 * DiffVec.size());
    return DiffMean;
}
double ConDist(std::vector<std::vector<double>> C1, std::vector<std::vector<double>> C2, int k){
    
    double OutVal;
    
    OutVal = ((std::pow((C1[k][0] - C2[k][0]), 2) /2) + (std::pow((C1[k][1]-C2[k][1]),2) /2));
    
    return OutVal;
}
double ConPhase(std::vector<std::vector<double>> C1, std::vector<std::vector<double>> C2, int k, double DiffMean){
    double OutVal;
    
    OutVal = (std::pow((C1[k][0]-C2[k][0] - DiffMean), 2)  +  std::pow((C1[k][1]-C2[k][1] - DiffMean), 2));
    
    return OutVal;
}
double ConStep(std::vector<std::vector<double>> C1, std::vector<std::vector<double>> C2, int k){
    double OutVal;
    
    OutVal = ((std::pow(((C1[k][0]-C1[k-1][1]) - (C2[k][0]-C2[k-1][1])),2) / 2)  + (std::pow( ((C1[k][1]-C1[k][0]) - (C2[k][1]-C2[k][0])) , 2) / 2));
    
    return OutVal;
}


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
                for(int j=indx2; j<Size2; j++)
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
void BestConsts(std::vector<int> Const1, std::vector<int> Const2, std::vector<std::vector<std::vector<int>>> &BestSet,
                std::vector<double> TSeqArr1, std::vector<double> TSeqDep1, std::vector<double> TSeqArr2, std::vector<double> TSeqDep2,
                std::vector<std::vector<double>> &tVec1, std::vector<std::vector<double>> &tVec2,
                double & BestDist, double & BestPhase, double & BestStep,
                std::vector<std::vector<double>> & DistLUT, std::vector<std::vector<double>> & DiffLUT, std::unordered_map<int, std::unordered_map<int, std::unordered_map<int, std::unordered_map<int, double>>>> & StepLUT,
                std::vector<double> & DistVec, std::vector<double> & PhaseVec, std::vector<double> & StepVec,
                double & CurrDist, double & CurrPhase, double & CurrStep){
    for(int m = 0; m<Const1.size(); m++){
        std::vector<double> TPair1(2, NAN);
        TPair1[0] = TSeqArr1[Const1[m]];
        TPair1[1] = TSeqDep1[Const1[m]];
        tVec1[m] = TPair1;
        std::vector<double> TPair2(2, NAN);
        TPair2[0] = TSeqArr2[Const2[m]];
        TPair2[1] = TSeqDep2[Const2[m]];
        tVec2[m] = TPair2;
    }
    
    double DiffMean = DiffM(tVec1, tVec2, Const1, Const2, DiffLUT);
    

    
    if(!(DistLUT[ Const1[0] ][ Const2[0] ] > 0)){
        DistVec[0] = ConDist(tVec1, tVec2, 0);
        DistLUT[Const1[0]][Const2[0]] = DistVec[0];
    } else{
        DistVec[0] = DistLUT[Const1[0]][Const2[0]];
    }
    
    PhaseVec[0] = ConPhase(tVec1, tVec2, 0, DiffMean);

    StepVec[0] = (0);
    
    
    
    for(int k = 1; k<Const1.size(); k++){
        
        if(!(DistLUT[Const1[k]][Const2[k]] > 0)){
            DistVec[k] = ConDist(tVec1, tVec2, k);;
            DistLUT[Const1[k]][Const2[k]] = DistVec[k];
        } else {
            DistVec[k] = DistLUT[Const1[k]][Const2[k]];
        }
        
        PhaseVec[k] = ConPhase(tVec1, tVec2, k, DiffMean);
        
        if(!(StepLUT[Const1[k-1]][Const1[k]][Const2[k-1]][Const2[k]] > 0)){
            StepVec[k] = ConStep(tVec1, tVec2, k);
            StepLUT[Const1[k-1]][Const1[k]][Const2[k-1]][Const2[k]] = StepVec[k];
        } else {
            StepVec[k] = StepLUT[Const1[k-1]][Const1[k]][Const2[k-1]][Const2[k]];
        }
        
    }
    
    CurrDist = std::accumulate(DistVec.begin(),DistVec.end(),0LL);
    CurrPhase = std::accumulate(PhaseVec.begin(),PhaseVec.end(),0LL);
    CurrStep = std::accumulate(StepVec.begin(),StepVec.end(),0LL);
    
    if(!(BestDist < CurrDist)){
        BestDist = CurrDist;
        for(int i = 0; i < DistVec.size(); i++){
            BestSet[0][0][i] = Const1[i];
            BestSet[0][1][i] = Const2[i];
        }
//                                             BestDistVec = DistVec;
    }
    if(!(BestPhase < CurrPhase)){
        BestPhase = CurrPhase;
        for(int i = 0; i < DistVec.size(); i++){
            BestSet[1][0][i] = Const1[i];
            BestSet[1][1][i] = Const2[i];
        }
//                                             BestPhaseVec = PhaseVec;
    }
    if(!(BestStep < CurrStep)){
        BestStep = CurrStep;
        for(int i = 0; i < DistVec.size(); i++){
            BestSet[2][0][i] = Const1[i];
            BestSet[2][1][i] = Const2[i];
        }
//                                             BestStepVec = StepVec;
    }
    //By this point, BestSet contains our best constructors. We can always come back and recalculate the values from here later.
    
    
}
void DFSInner(std::vector<std::vector<int>> Graph, std::vector<int> StartKeys, std::vector<int> EndKeys, std::vector<std::vector<int>> DKey, int lcslen,
              // Housekeeping arguments to pass to the core
              std::vector<int> Const1, std::vector<int> & Const2, std::vector<std::vector<std::vector<int>>> &BestSet, std::vector<double> TSeqArr1, std::vector<double> TSeqDep1, std::vector<double> TSeqArr2, std::vector<double> TSeqDep2, std::vector<std::vector<double>> &tVec1, std::vector<std::vector<double>> &tVec2, double & BestDist, double & BestPhase, double & BestStep, std::vector<std::vector<double>> & DistLUT, std::vector<std::vector<double>> & DiffLUT, std::unordered_map<int, std::unordered_map<int, std::unordered_map<int, std::unordered_map<int, double>>>> & StepLUT, std::vector<double> & DistVec, std::vector<double> & PhaseVec, std::vector<double> & StepVec, double & CurrDist, double & CurrPhase, double & CurrStep){
    int MaxDepth = lcslen - 1;
    
    std::vector<int> NodeVec(DKey[0].size(), NAN);
    
    for(int i = 0; i < StartKeys.size(); i++){
            
            std::stack<int> s;
            
            s.push(StartKeys[i]);
        
            while(!s.empty()){
                int Curr = s.top();
                s.pop();

                int Depth = DKey[1][Curr];
                
                Const2[Depth] = DKey[2][Curr];
                
                    if(Depth == MaxDepth){
                        
                        BestConsts(Const1, Const2, BestSet, TSeqArr1, TSeqDep1, TSeqArr2, TSeqDep2, tVec1, tVec2, BestDist, BestPhase, BestStep, DistLUT, DiffLUT, StepLUT, DistVec, PhaseVec, StepVec, CurrDist, CurrPhase, CurrStep);
                    
                    } else {
                
                for(int k = 0; k < Graph[Curr].size(); k++){
                    s.push(Graph[Curr][k]);
                }
                }
                }
            }
}
void DFSOuter(std::vector<std::vector<int>> Graph, std::vector<int> StartKeys, std::vector<int> EndKeys, std::vector<std::vector<int>> DKey, int lcslen,
              // Housekeeping arguments to pass to the inner loop
              std::vector<std::vector<int>> Graph2, std::vector<int> StartKeys2, std::vector<int> EndKeys2, std::vector<std::vector<int>> DKey2,
              // Housekeeping arguments to pass to the core
              std::vector<int> & Const1, std::vector<int> & Const2, std::vector<std::vector<std::vector<int>>> &BestSet, std::vector<double> TSeqArr1, std::vector<double> TSeqDep1, std::vector<double> TSeqArr2, std::vector<double> TSeqDep2, std::vector<std::vector<double>> & tVec1, std::vector<std::vector<double>> & tVec2, double & BestDist, double & BestPhase, double & BestStep, std::vector<std::vector<double>> & DistLUT, std::vector<std::vector<double>> & DiffLUT, std::unordered_map<int, std::unordered_map<int, std::unordered_map<int, std::unordered_map<int, double>>>> & StepLUT, std::vector<double> & DistVec, std::vector<double> & PhaseVec, std::vector<double> & StepVec, double & CurrDist, double & CurrPhase, double & CurrStep){
    int MaxDepth = lcslen - 1;
    std::vector<int> NodeVec(DKey[0].size(), NAN);
    for(int i = 0; i < StartKeys.size(); i++){
            
            std::stack<int> s;
            
            s.push(StartKeys[i]);
        
            while(!s.empty()){
                int Curr = s.top();
                s.pop();

                int Depth = DKey[1][Curr];
                
                Const1[Depth] = DKey[2][Curr];
                
                    if(Depth == MaxDepth){
                        
                        DFSInner(Graph2, StartKeys2, EndKeys2, DKey2, lcslen, Const1, Const2, BestSet, TSeqArr1, TSeqDep1, TSeqArr2, TSeqDep2, tVec1, tVec2, BestDist, BestPhase, BestStep, DistLUT, DiffLUT, StepLUT, DistVec, PhaseVec, StepVec, CurrDist, CurrPhase, CurrPhase);
                    
                    } else {
                
                for(int k = 0; k < Graph[Curr].size(); k++){
                    s.push(Graph[Curr][k]);
                }
                }
                }
            }
    
    
    
}

//void BestConstsSub(std::vector<int> Const1, std::vector<int> Const2,
//                std::vector<double> TSeqArr1, std::vector<double> TSeqDep1, std::vector<double> TSeqArr2, std::vector<double> TSeqDep2,
//                std::vector<std::vector<double>> &tVec1, std::vector<std::vector<double>> &tVec2,
//                std::vector<std::vector<double>> & DistLUT, std::vector<std::vector<double>> & DiffLUT, std::unordered_map<int, std::unordered_map<int, std::unordered_map<int, std::unordered_map<int, double>>>> & StepLUT,
//                std::vector<double> & DistVec, std::vector<double> & PhaseVec, std::vector<double> & StepVec,
//                double & CurrDist, double & CurrPhase, double & CurrStep, std::unordered_map<int, std::unordered_map<int,std::unordered_map<int, std::unordered_map<int, std::unordered_map<int, std::tuple<double, std::vector<std::vector<int>>>>>>>> & BestSub){
//    for(int m = 0; m<Const1.size(); m++){
//        std::vector<double> TPair1(2, NAN);
//        TPair1[0] = TSeqArr1[Const1[m]];
//        TPair1[1] = TSeqDep1[Const2[m]];
//        tVec1[m] = TPair1;
//        std::vector<double> TPair2(2, NAN);
//        TPair2[0] = TSeqArr2[Const1[m]];
//        TPair2[1] = TSeqDep2[Const2[m]];
//        tVec2[m] = TPair2;
//    }
//
//    double DiffMean = DiffM(tVec1, tVec2, Const1, Const2, DiffLUT);
//    int L = Const1.size();
//
//
//    if(!(DistLUT[ Const1[0] ][ Const2[0] ] > 0)){
//        DistVec[0] = ConDist(tVec1, tVec2, 0);
//        DistLUT[Const1[0]][Const2[0]] = DistVec[0];
//    } else{
//        DistVec[0] = DistLUT[Const1[0]][Const2[0]];
//    }
//
//    PhaseVec[0] = ConPhase(tVec1, tVec2, 0, DiffMean);
//
//    StepVec[0] = (0);
//
//
//
//    for(int k = 1; k<Const1.size(); k++){
//
//        if(!(DistLUT[Const1[k]][Const2[k]] > 0)){
//            DistVec[k] = ConDist(tVec1, tVec2, k);;
//            DistLUT[Const1[k]][Const2[k]] = DistVec[k];
//        } else {
//            DistVec[k] = DistLUT[Const1[k]][Const2[k]];
//        }
//
//        PhaseVec[k] = ConPhase(tVec1, tVec2, k, DiffMean);
//
//        if(!(StepLUT[Const1[k-1]][Const1[k]][Const2[k-1]][Const2[k]] > 0)){
//            StepVec[k] = ConStep(tVec1, tVec2, k);
//            StepLUT[Const1[k-1]][Const1[k]][Const2[k-1]][Const2[k]] = StepVec[k];
//        } else {
//            StepVec[k] = StepLUT[Const1[k-1]][Const1[k]][Const2[k-1]][Const2[k]];
//        }
//
//    }
//
//    CurrDist = std::accumulate(DistVec.begin(),DistVec.end(),0LL);
//    CurrPhase = std::accumulate(PhaseVec.begin(),PhaseVec.end(),0LL);
//    CurrStep = std::accumulate(StepVec.begin(),StepVec.end(),0LL);
//
//    if(std::get<0>(BestSub[Const1[0]][Const1[L]][Const2[0]][Const2[L]][0]) == 0){
//       std::get<0>(BestSub[Const1[0]][Const1[L]][Const2[0]][Const2[L]][0]) = NAN;
//       std::get<0>(BestSub[Const1[0]][Const1[L]][Const2[0]][Const2[L]][1]) = NAN;
//       std::get<0>(BestSub[Const1[0]][Const1[L]][Const2[0]][Const2[L]][2]) = NAN;
//    }
//
//
//
//
//
//    if(!(std::get<0>(BestSub[Const1[0]][Const1[L]][Const2[0]][Const2[L]][0]) < CurrDist)){
//
//        std::get<0>(BestSub[Const1[0]][Const1[L]][Const2[0]][Const2[L]][0]) = CurrDist;
//
//        std::get<1>(BestSub[Const1[0]][Const1[L]][Const2[0]][Const2[L]][0]).resize(2);
//        std::get<1>(BestSub[Const1[0]][Const1[L]][Const2[0]][Const2[L]][0])[0].assign(Const1.begin(), Const1.end());
//        std::get<1>(BestSub[Const1[0]][Const1[L]][Const2[0]][Const2[L]][0])[1].assign(Const2.begin(), Const2.end());
////                                             BestDistVec = DistVec;
//    }
//    if(!(std::get<0>(BestSub[Const1[0]][Const1[L]][Const2[0]][Const2[L]][1]) < CurrPhase)){
//
//        std::get<0>(BestSub[Const1[0]][Const1[L]][Const2[0]][Const2[L]][1]) = CurrPhase;
//
//        std::get<1>(BestSub[Const1[0]][Const1[L]][Const2[0]][Const2[L]][1]).resize(2);
//        std::get<1>(BestSub[Const1[0]][Const1[L]][Const2[0]][Const2[L]][1])[0].assign(Const1.begin(), Const1.end());
//        std::get<1>(BestSub[Const1[0]][Const1[L]][Const2[0]][Const2[L]][1])[1].assign(Const2.begin(), Const2.end());
////                                             BestPhaseVec = PhaseVec;
//    }
//    if(!(std::get<0>(BestSub[Const1[0]][Const1[L]][Const2[0]][Const2[L]][2]) < CurrStep)){
//
//        std::get<0>(BestSub[Const1[0]][Const1[L]][Const2[0]][Const2[L]][2]) = CurrStep;
//
//        std::get<1>(BestSub[Const1[0]][Const1[L]][Const2[0]][Const2[L]][2]).resize(2);
//        std::get<1>(BestSub[Const1[0]][Const1[L]][Const2[0]][Const2[L]][2])[0].assign(Const1.begin(), Const1.end());
//        std::get<1>(BestSub[Const1[0]][Const1[L]][Const2[0]][Const2[L]][2])[1].assign(Const2.begin(), Const2.end());
////                                             BestStepVec = StepVec;
//    }
//    //By this point, BestSet contains our best constructors. We can always come back and recalculate the values from here later.
//
//
//}
//void DFSInnerSub(std::vector<std::vector<int>> Graph, std::vector<int> StartKeys, std::vector<int> EndKeys, std::vector<std::vector<int>> DKey,
//                 // Housekeeping arguments to pass to the core
//                 std::vector<int> Const1, std::vector<int> & Const2, std::vector<double> TSeqArr1, std::vector<double> TSeqDep1, std::vector<double> TSeqArr2, std::vector<double> TSeqDep2, std::vector<std::vector<double>> &tVec1, std::vector<std::vector<double>> &tVec2, std::vector<std::vector<double>> & DistLUT, std::vector<std::vector<double>> & DiffLUT, std::unordered_map<int, std::unordered_map<int, std::unordered_map<int, std::unordered_map<int, double>>>> & StepLUT, std::vector<double> & DistVec, std::vector<double> & PhaseVec, std::vector<double> & StepVec, double & CurrDist, double & CurrPhase, double & CurrStep, std::unordered_map<int, std::unordered_map<int,std::unordered_map<int, std::unordered_map<int, std::unordered_map<int, std::tuple<double, std::vector<std::vector<int>>>>>>>> & BestSub){
//    int MaxDepth = *max_element(DKey[1].begin(),DKey[1].end());
//
//    std::vector<int> NodeVec(DKey[0].size(), NAN);
//    // On the out here, we can build our vectors to pass.
//
//
//    for(int i = 0; i < StartKeys.size(); i++){
//
//            std::stack<int> s;
//
//            s.push(StartKeys[i]);
//
//            while(!s.empty()){
//                int Curr = s.top();
//                s.pop();
//
//                int Depth = DKey[1][Curr];
//
//                Const2[Depth] = DKey[2][Curr];
//
//                    if(Depth == MaxDepth){
//
//                        BestConstsSub(Const1, Const2, TSeqArr1, TSeqDep1, TSeqArr2, TSeqDep2, tVec1, tVec2, DistLUT, DiffLUT, StepLUT, DistVec, PhaseVec, StepVec, CurrDist, CurrPhase, CurrStep, BestSub);
//
//                    } else {
//
//                for(int k = 0; k < Graph[Curr].size(); k++){
//                    s.push(Graph[Curr][k]);
//                }
//                }
//                }
//            }
//}
//void DFSOuterSub(std::vector<std::vector<int>> Graph, std::vector<int> StartKeys, std::vector<int> EndKeys, std::vector<std::vector<int>> DKey,
//              // Housekeeping arguments to pass to the inner loop
//              std::vector<std::vector<int>> Graph2, std::vector<int> StartKeys2, std::vector<int> EndKeys2, std::vector<std::vector<int>> DKey2,
//              // Housekeeping arguments to pass to the core
//              std::vector<double> TSeqArr1, std::vector<double> TSeqDep1, std::vector<double> TSeqArr2, std::vector<double> TSeqDep2, std::vector<std::vector<double>> & DistLUT, std::vector<std::vector<double>> & DiffLUT, std::unordered_map<int, std::unordered_map<int, std::unordered_map<int, std::unordered_map<int, double>>>> & StepLUT, std::unordered_map<int, std::unordered_map<int,std::unordered_map<int, std::unordered_map<int, std::unordered_map<int, std::tuple<double, std::vector<std::vector<int>>>>>>>> & BestSub){
//    int MaxDepth = *max_element(DKey[1].begin(),DKey[1].end());
//    std::vector<int> NodeVec(DKey[0].size(), NAN);
//    std::vector<int> Const1(MaxDepth+1, NAN);
//    std::vector<int> Const2(MaxDepth+1, NAN);
//    std::vector<double> DistVec(MaxDepth+1, NAN);
//    std::vector<double> PhaseVec(MaxDepth+1, NAN);
//    std::vector<double> StepVec(MaxDepth+1, NAN);
//    std::vector<std::vector<double>> tVec1(MaxDepth+1, std::vector<double>(2, NAN));
//    std::vector<std::vector<double>> tVec2(MaxDepth+1, std::vector<double>(2, NAN));
//    double CurrDist;
//    double CurrPhase;
//    double CurrStep;
//
//    for(int i = 0; i < StartKeys.size(); i++){
//
//            std::stack<int> s;
//
//            s.push(StartKeys[i]);
//
//            while(!s.empty()){
//                int Curr = s.top();
//                s.pop();
//
//                int Depth = DKey[1][Curr];
//
//                Const1[Depth] = DKey[2][Curr];
//
//                    if(Depth == MaxDepth){
//
//                        DFSInnerSub(Graph2, StartKeys2, EndKeys2, DKey2, Const1, Const2, TSeqArr1, TSeqDep1, TSeqArr2, TSeqDep2, tVec1, tVec2, DistLUT, DiffLUT, StepLUT, DistVec, PhaseVec, StepVec, CurrDist, CurrPhase, CurrStep, BestSub);
//
//                    } else {
//
//                for(int k = 0; k < Graph[Curr].size(); k++){
//                    s.push(Graph[Curr][k]);
//                }
//                }
//                }
//            }
//
//
//
//}
/*
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
 */

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
/*
 if(NodeVec[curr] > -1){
     
     Const1[curr] = int(NodeVec[curr]);
     
 } else {
     
     auto NInd = find(DKey[0].begin(), DKey[0].end(), curr);
     int NodeC = DKey[2][NInd - DKey[0].begin()];
     NodeVec[curr] = double(NodeC);
     Const1[Curr] = NodeC;
     
 }
 Const1[Depth] = Curr;
 */

void Graphify(std::vector<std::vector<int>> &Graph, std::vector<std::vector<int>> &DKey,
              std::vector<std::vector<int>> FMT){
    // Function takes an empty Graph object and an empty DKey (of length 3) and uses a match table to populate them for finding viable traversals.
    std::vector<int> N;
    std::vector<int> D;
    std::vector<int> C;
    std::vector<std::vector<int>> CKey(FMT);
    int Gind = 0;
    for(int j = 0; j < FMT.size(); j++){
        for(int k = 0; k < FMT[j].size(); k++){
            CKey[j][k] = Gind;
            N.push_back(Gind);
            D.push_back(j);
            C.push_back(FMT[j][k]);
            Gind++;
            }
    }
    DKey[0] = N;
    DKey[1] = D;
    DKey[2] = C;

    int jLim = (FMT.size()-1);
    for(int j = 0; j<(FMT.size()); j++){
        for(int k = 0; k<FMT[j].size(); k++){

            int CPar = FMT[j][k];

            std::vector<int> CAdj;
            if(jLim != j){

            for(int m = 0; m<FMT[j+1].size(); m++){
                int CChil = FMT[j+1][m];
                if(CPar < CChil){
                    CAdj.push_back(CKey[j+1][m]);
                }
            }}
            Graph.push_back(CAdj);

        }
    }

}
//
//void FMPruner(std::vector<std::vector<int>> & FMT1, std::vector<std::vector<int>> & FMT2, std::vector<double> TSeqArr1, std::vector<double> TSeqDep1, std::vector<double> TSeqArr2, std::vector<double> TSeqDep2, std::vector<std::vector<double>> & DistLUT, std::vector<std::vector<double>> & DiffLUT, std::unordered_map<int, std::unordered_map<int, std::unordered_map<int, std::unordered_map<int, double>>>> & StepLUT, int PruneThreshC, int PruneThreshLev){
//    // This nutty hash table has as its first key, the first starting index, as its second key, the second starting index, and, as its third key, an integer from 0 to 2 indicating which distance metric it refers to.
//
//    std::vector<int> Levs(FMT1.size(), 0);
//    std::vector<int> ContKey(2);
//    std::vector<std::vector<int>> ContTable;
//
//
//    if((FMT1[0].size() >= PruneThreshLev) || (FMT2[0].size() >= PruneThreshLev)){
//        ContKey[0] = 0;
//    }
//    for(int i = 1; i < Levs.size(); i++){
//        if((FMT1[i].size() < PruneThreshLev) || (FMT2[i].size() < PruneThreshLev)){
//            if(Levs[i-1] != 0){
//                // End a sequence of threshold meeters
//                ContKey[1] = (i - ContKey[0]);
//                ContTable.push_back(ContKey);
//            }
//
//        } else {
//            Levs[i] = 1;
//            if(Levs[i-1] == 0){
//                // Start a new sequence of threshold meeters.
//                ContKey[0] = i;
//            }
//        }
//    }
//
//
//    std::vector<std::vector<int>> DKey1(3);
//    std::vector<std::vector<int>> DKey2(3);
//    std::vector<std::vector<int>> Graph1;
//    std::vector<std::vector<int>> Graph2;
//    std::vector<std::vector<int>> Keys1;
//    std::vector<std::vector<int>> Keys2;
//    int Start;
//    int End;
//
//    for(int i = 0; i < ContTable.size(); i++){
//        if(ContTable[i][1] >= PruneThreshC){
//            std::unordered_map<int, std::unordered_map<int,std::unordered_map<int, std::unordered_map<int, std::unordered_map<int, std::tuple<double, std::vector<std::vector<int>>>>>>>> BestSub;
//
//
//            DKey1.clear();
//            DKey2.clear();
//            DKey1.resize(3);
//            DKey2.resize(3);
//            Graph1.clear();
//            Graph2.clear();
//
//            if(ContTable[i][0] == 0){
//                Start = 0;
//            } else {
//                Start = ContTable[i][0] - 1;
//            }
//            if((ContTable[i][0] + ContTable[i][1] + 1) < (Levs.size() - 1)){
//                End = ContTable[i][0] + ContTable[i][1] + 1;
//            } else {
//                End = Levs.size() - 1;
//            }
//
//            auto FMT1S = FMT1.begin() + Start;
//            auto FMT1E = FMT1.begin() + End;
//            std::vector<std::vector<int>> FMT1Sub(FMT1S, FMT1E);
//            auto FMT2S = FMT2.begin() + Start;
//            auto FMT2E = FMT2.begin() + End;
//            std::vector<std::vector<int>> FMT2Sub(FMT2S, FMT2E);
//
//            Graphify(Graph1, DKey1, FMT1Sub);
//            Graphify(Graph2, DKey2, FMT2Sub);
//            Keys1 = KeyFinder(DKey1);
//            Keys2 = KeyFinder(DKey2);
//
//            DFSOuterSub(Graph1, Keys1[0], Keys1[1], DKey1, Graph2, Keys2[0], Keys2[1], DKey2, TSeqArr1, TSeqDep1, TSeqArr2, TSeqDep2, DistLUT, DiffLUT, StepLUT, BestSub);
//
//
//            std::vector<std::set<int>> PruneSet1 (End - Start + 1);
//            std::vector<std::set<int>> PruneSet2 (End - Start + 1);
//            // In this loop, we can run through our hash table based on the indices we saved and start to prune our tree.
//            for(int j = 0; j < Keys1[0].size(); j++){
//                for(int k = 0; k < Keys1[1].size(); k++){
//                    for(int l = 0; l < Keys2[0].size(); l++){
//                        for(int m = 0; m < Keys2[1].size(); m++){
//                            // Here we check the values from the hash table and push them into a pair of sets of positions to "prune" our match table by.
//                            if(std::get<0>(BestSub[Keys1[0][j]][Keys1[1][k]][Keys2[0][l]][Keys2[1][m]][0]) != 0){
//                               for(int n = 0; n < 3; n++){
//                                    std::vector<std::vector<int>> ToSave = std::get<1>(BestSub[Keys1[0][j]][Keys1[1][k]][Keys2[0][l]][Keys2[1][m]][n]);
//                                    for(int o = 0; o < ToSave[0].size(); o++){
//                                        PruneSet1[o].insert(ToSave[0][o]);
//                                        PruneSet2[o].insert(ToSave[1][o]);
//                                    }
//                                }
//                            }
//                        }
//                    }
//                }
//            }
//            // Now we can overwrite our match tables by assigning vectors at positions from our sets.
//            for(int j = Start; j < End+1; j++){
//                int PKey = j - Start;
//                FMT1[j].assign(PruneSet1[PKey].begin(), PruneSet1[PKey].end());
//                FMT2[j].assign(PruneSet2[PKey].begin(), PruneSet2[PKey].end());
//            }
//
//        }
//    }
//    // And bob's your uncle!
//
//
//
//}



void LCSConstructHunt(std::vector<std::vector<int>> LCSset, std::vector<int> Fish1Seq, std::vector<int> Fish2Seq, std::vector<std::tuple<std::vector<std::string>,std::vector<std::vector<double>>,std::vector<std::vector<double>>,double>> & PullSet, std::vector<double> TSeqArr1, std::vector<double> TSeqDep1, std::vector<double> TSeqArr2, std::vector<double> TSeqDep2, std::vector<std::string> Method, std::vector<std::string> LocLib, int lcslen, int Size1, int Size2, int PruneThreshC, int PruneThreshLev){
    std::vector<std::vector<std::tuple<std::vector<std::string>,std::vector<std::vector<double>>,std::vector<std::vector<double>>,double>>> Candidates(Method.size());
    
    
    

    

    
    
    
    
    std::vector<std::vector<double>> DistLUT (Size1, std::vector<double> (Size2, NAN));
    std::vector<std::vector<double>> DiffLUT (Size1, std::vector<double> (Size2, NAN));
    std::unordered_map<int, std::unordered_map<int, std::unordered_map<int, std::unordered_map<int, double>>>> StepLUT;
    /*
    std::vector<std::vector<std::vector<std::vector<double>>>> StepLUT (Size1, std::vector<std::vector<std::vector<double>>>(Size1, std::vector<std::vector<double>>(Size2,std::vector<double>(Size2, NAN))));
    */
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

        
        // We need to prune somewhere in here.
        //FMPruner(FMT1, FMT2, TSeqArr1, TSeqDep1, TSeqArr2, TSeqDep2, DistLUT, DiffLUT, StepLUT, PruneThreshC, PruneThreshLev);

        // Remember, this is still in a for loop to step through each LCS! Stepping through each constructor is done by the Construct Stepper
        std::vector<std::vector<int>> DKey1(3);
        std::vector<std::vector<int>> DKey2(3);

        
        std::vector<std::vector<int>> CGraph1;
        std::vector<std::vector<int>> CGraph2;
        
        
        Graphify(CGraph1, DKey1, FMT1);
        Graphify(CGraph2, DKey2, FMT2);

        
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
        std::vector<std::vector<std::vector<int>>> BestSet(3, std::vector<std::vector<int>>(2,std::vector<int>(lcslen,NAN)));
        std::vector<std::vector<double>> tVec1(lcslen, std::vector<double> (2, NAN));
        std::vector<std::vector<double>> tVec2(lcslen, std::vector<double> (2, NAN));
        double BestDist = NAN;
        double BestPhase = NAN;
        double BestStep = NAN;
        std::vector<double> DistVec (lcslen, NAN);
        std::vector<double> PhaseVec (lcslen, NAN);
        std::vector<double> StepVec (lcslen, NAN);
        double CurrDist = NAN;
        double CurrPhase = NAN;
        double CurrStep = NAN;
        std::vector<int> Const1_(lcslen);
        std::vector<int> Const2_(lcslen);

        DFSOuter(CGraph1, Keys1[0], Keys1[1], DKey1, lcslen, CGraph2, Keys2[0], Keys2[1], DKey2, Const1_, Const2_, BestSet, TSeqArr1, TSeqDep1, TSeqArr2, TSeqDep2, tVec1, tVec2, BestDist, BestPhase, BestStep, DistLUT, DiffLUT, StepLUT, DistVec, PhaseVec, StepVec, CurrDist, CurrPhase, CurrStep);


        std::vector<std::string> LCSString = LCSStringify(LCSset[i], lcslen, LocLib);
        
        
                     //End of the loop through each LCS, save the least squares distance through time for each "best constructor" for each LCS
        for(int k = 0; k < 3; k++){
            std::vector<std::vector<double>> tOutVec1(lcslen, std::vector<double>(2,NAN));
            std::vector<std::vector<double>> tOutVec2(lcslen, std::vector<double>(2,NAN));
            for(int l = 0; l < lcslen; l++){
                tOutVec1[l][0] = TSeqArr1[BestSet[k][0][l]];
                tOutVec1[l][1] = TSeqDep1[BestSet[k][0][l]];
                tOutVec2[l][0] = TSeqArr2[BestSet[k][1][l]];
                tOutVec2[l][1] = TSeqDep2[BestSet[k][1][l]];
            }
            std::vector<double> OutDissim (lcslen, NAN);
            double Met = NAN;
            double MetSum = NAN;
            if(k == 0){
                for(int l = 0; l<lcslen; l++){
                    
                    if(!(DistLUT[BestSet[k][0][l]][BestSet[k][1][l]] > 0)){
                        OutDissim[l] = ConDist(tOutVec1, tOutVec2, l);;
                        DistLUT[BestSet[k][0][l]][BestSet[k][1][l]] = OutDissim[l];
                    } else {
                        OutDissim[l] = DistLUT[BestSet[k][0][l]][BestSet[k][1][l]];
                    }
                }
                    
            } else if(k == 1){
                double DiffMeanOut = DiffM(tOutVec1, tOutVec2, BestSet[k][0], BestSet[k][1], DiffLUT);
                for(int l = 0; l<lcslen; l++){
                    OutDissim[l] = ConPhase(tOutVec1, tOutVec2, l, DiffMeanOut);
                }
                
            } else if(k == 2){
                OutDissim[0] = 0;
                for(int l = 1; l<lcslen; l++){
                    if(!(StepLUT[BestSet[k][0][l-1]][BestSet[k][0][l]][BestSet[k][1][l-1]][BestSet[k][1][l]] > 0)){
                        OutDissim[l] = ConStep(tOutVec1, tOutVec2, l);
                        StepLUT[BestSet[k][0][l-1]][BestSet[k][0][l]][BestSet[k][1][l-1]][BestSet[k][1][l]] = OutDissim[l];
                    } else {
                        OutDissim[l] = StepLUT[BestSet[k][0][l-1]][BestSet[k][0][l]][BestSet[k][1][l-1]][BestSet[k][1][l]];
                    }
                }
            }
            
            MetSum = std::accumulate(OutDissim.begin(), OutDissim.end(), 0LL);
            Met = (MetSum / OutDissim.size());

            
                 std::tuple<std::vector<std::string>,std::vector<std::vector<double>>,std::vector<std::vector<double>>,double> CombinedConst = std::make_tuple(LCSString, tOutVec1, tOutVec2, Met);
                 

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
        
    auto BestTarget = std::min_element(AggDist.begin(), AggDist.end());
    int BestCand = std::distance(AggDist.begin(),BestTarget);
    PullSet.push_back(Candidates[k][BestCand]);
    } else {
        PullSet.push_back(Candidates[k][0]);
    }
    }
    return;
}



std::vector<std::tuple<std::vector<std::string>,std::vector<std::vector<double>>,std::vector<std::vector<double>>,double>> FullLCSExtractor(std::vector<int> Fish1Seq, std::vector<int> Fish2Seq/*, int argc, char** argv*/, std::vector<double> TSeqArr1, std::vector<double> TSeqDep1, std::vector<double> TSeqArr2, std::vector<double> TSeqDep2, std::vector<std::string> Method, std::vector<std::string> LocLib, int PruneThreshC, int PruneThreshLev){
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
    // PullAll is doing something funky and I have no clue why... so we're going to cheat bit using a set.
    
    std::set<std::vector<int>> TempLCS;
    for(auto it = LCSset.begin(); it < LCSset.end(); it++){
        TempLCS.insert(*it);
    }
    
    LCSset.clear();
    
    for(auto it = TempLCS.begin(); it != TempLCS.end(); it++){
        LCSset.push_back(*it);
    }
    
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
    LCSConstructHunt(LCSset, Fish1Seq, Fish2Seq, PullSet, TSeqArr1, TSeqDep1, TSeqArr2, TSeqDep2, Methods, LocLib, lcslen, Size1, Size2, PruneThreshC, PruneThreshLev);
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
    
    int PruneThreshC = 5;
    int PruneThreshLev = 4;
    
    IntLibConverter(Fish1Seq_,Fish2Seq_,Fish1Seq,Fish2Seq,LocLib);
    //std::string dbm = as<String>(Debug);
   /* Rcpp::CharacterVector DbM(Debug);*/
  //  Rcpp::Rcout << Fish1Seq[0] << Fish1Seq[1] << Fish1Seq[2] << Fish1Seq[3] <<  "\n";

    
    // step 1: call the underlying C++ function
    std::vector<std::tuple<std::vector<std::string>,std::vector<std::vector<double>>,std::vector<std::vector<double>>,double>> LeastComm = FullLCSExtractor(Fish1Seq, Fish2Seq, TSeqArr1, TSeqDep1, TSeqArr2, TSeqDep2, Method, LocLib, PruneThreshC, PruneThreshLev);

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
