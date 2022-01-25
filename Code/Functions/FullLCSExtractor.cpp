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
void LCSConstructStepper(std::vector<int> LCS, std::vector<std::vector<int>> FishMatchTable, std::vector<std::vector<int>> & ConstrSet, std::vector<int> & ConstrVec, int RunDex, int currlcs, int lcslen){
    // This part is really slow due to the recursion, but I'm not sure how best to speed it up.
    if(currlcs == lcslen){
        ConstrSet.push_back(ConstrVec);
        return;
    }
    // Remember that this is inside a for loop using i, but it shouldn't cause an issue to loop like this anyway
    for(int j = 0; j<FishMatchTable[RunDex].size(); j++){
        ConstrVec[RunDex] = FishMatchTable[RunDex][j];
        if(RunDex == 0 || ConstrVec[RunDex] > ConstrVec[RunDex-1]){
            LCSConstructStepper(LCS, FishMatchTable, ConstrSet, ConstrVec, RunDex + 1, currlcs + 1, lcslen);
        }
    }
    return;
}



std::vector<std::string> LCSStringify(std::vector<int> LCSInt, int lcslen, std::vector<std::string> LocLib){
    std::vector<std::string> LCSString;
    for(int k = 0; k < lcslen; k++){
        LCSString.push_back(LocLib[LCSInt[k]]);
    }
    return(LCSString);
}


void LCSConstructHunt(std::vector<std::vector<int>> LCSset, std::vector<int> Fish1Seq, std::vector<int> Fish2Seq, std::vector<std::tuple<std::vector<std::string>,std::vector<std::vector<double>>,std::vector<std::vector<double>>,double>> & PullSet, std::vector<double> TSeqArr1, std::vector<double> TSeqDep1, std::vector<double> TSeqArr2, std::vector<double> TSeqDep2, std::vector<std::string> Method, std::vector<std::string> LocLib, int lcslen){
    std::vector<std::vector<std::tuple<std::vector<std::string>,std::vector<std::vector<double>>,std::vector<std::vector<double>>,double>>> Candidates(Method.size());
    
    int l = Method.size();
    int BestConsts[l][2];
    
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
        LCSConstructStepper(LCSset[i], FMT1, CSet1, Const1_, 0, 0, lcslen);
        LCSConstructStepper(LCSset[i], FMT2, CSet2, Const2_, 0, 0, lcslen);
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
            std::vector<std::vector<std::vector<double>>> ConstrTable1;
            std::vector<std::vector<std::vector<double>>> ConstrTable2;
            
                 for(int i = 0; i<CSet1.size(); i++){
                     std::vector<std::vector<double>> TPairVec1;
                     
                     for(int j = 0; j<CSet1[0].size(); j++){
                         std::vector<double> TPair1;
                         TPair1.push_back(TSeqArr1[CSet1[i][j]]);
                         TPair1.push_back(TSeqDep1[CSet1[i][j]]);
                         TPairVec1.push_back(TPair1);
                     }
                     ConstrTable1.push_back(TPairVec1);
                 }
                 
                 for(int i = 0; i<CSet2.size(); i++){
                     std::vector<std::vector<double>> TPairVec2;
                     
                     for(int j = 0; j<CSet2[0].size(); j++){
                         std::vector<double> TPair2;
                         TPair2.push_back(TSeqArr2[CSet2[i][j]]);
                         TPair2.push_back(TSeqDep2[CSet2[i][j]]);
                         TPairVec2.push_back(TPair2);
                     }
                     ConstrTable2.push_back(TPairVec2);
                 }
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
                        std::vector<double> DistVec;
                        std::vector<double> PhaseVec;
                        std::vector<double> StepVec;
                        tVec2 = ConstrTable2[j];
                        
                        std::vector<double> DiffVec;
                        double DiffMean;
                        for(int k = 0; k<ConstrTable1[0].size(); k++){
                            DiffVec.push_back((tVec1[k][0] - tVec2[k][0]) + (tVec1[k][1] - tVec2[k][1]));
                        }
                        
                        double DiffSum = std::accumulate(DiffVec.begin(), DiffVec.end(), 0LL);
                        DiffMean = DiffSum / (2 * DiffVec.size());
                        
                        DistVec.push_back((std::pow((tVec1[0][0] - tVec2[0][0]), 2) /2) + (std::pow((tVec1[0][1]-tVec2[0][1]),2) /2));
                        
                        PhaseVec.push_back(std::pow((tVec1[0][0]-tVec2[0][0] - DiffMean), 2)  +  std::pow((tVec1[0][1]-tVec2[0][1] - DiffMean), 2));
                        
                        StepVec.push_back(0);
                        
                        
                        
                        for(int k = 1; k<ConstrTable1[0].size(); k++){
                            DistVec.push_back((std::pow((tVec1[k][0] - tVec2[k][0]), 2) /2) + (std::pow((tVec1[k][1]-tVec2[k][1]),2) /2));
                            
                            PhaseVec.push_back(std::pow((tVec1[k][0]-tVec2[k][0] - DiffMean), 2)  +  std::pow((tVec1[k][1]-tVec2[k][1] - DiffMean), 2));
                            
                            StepVec.push_back((std::pow((tVec1[k][0]-tVec1[k-1][1]) - (tVec2[k][0]-tVec2[k-1][1]),2)/2)  + (std::pow((tVec1[k][1]-tVec1[k][0]) - (tVec2[k][1]-tVec2[k][0]), 2)/2));
                        }
                        
                        BestDist = std::accumulate(DistVec.begin(), DistVec.end(), 0);

                        BestPhase = std::accumulate(PhaseVec.begin(), PhaseVec.end(), 0);

                        BestStep = std::accumulate(StepVec.begin(), StepVec.end(), 0);

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

                                         std::vector<std::vector<double>> tVec1;

                                         std::vector<std::vector<double>> tVec2;

                                             std::vector<double> DistVec;
                                             std::vector<double> PhaseVec;
                                             std::vector<double> StepVec;
                                                 for(int m = 0; m<CSet1[0].size(); m++){
                                                     std::vector<double> TPair1;
                                                     TPair1.push_back(TSeqArr1[CSet1[i][m]]);
                                                     TPair1.push_back(TSeqDep1[CSet1[i][m]]);
                                                     tVec1.push_back(TPair1);
                                                     std::vector<double> TPair2;
                                                     TPair2.push_back(TSeqArr2[CSet2[j][m]]);
                                                     TPair2.push_back(TSeqDep2[CSet2[j][m]]);
                                                     tVec2.push_back(TPair2);
                                                 }
                                             
                                             std::vector<double> DiffVec;
                                             double DiffMean;
                                             for(int k = 0; k<tVec1.size(); k++){
                                                 DiffVec.push_back((tVec1[k][0] - tVec2[k][0]) + (tVec1[k][1] - tVec2[k][1]));
                                             }
                                             
                                             double DiffSum = std::accumulate(DiffVec.begin(), DiffVec.end(), 0LL);
                                             DiffMean = DiffSum / (2 * DiffVec.size());
                                             
                                             DistVec.push_back((std::pow((tVec1[0][0] - tVec2[0][0]), 2) /2) + (std::pow((tVec1[0][1]-tVec2[0][1]),2) /2));
                                             
                                             PhaseVec.push_back(std::pow((tVec1[0][0]-tVec2[0][0] - DiffMean), 2)  +  std::pow((tVec1[0][1]-tVec2[0][1] - DiffMean), 2));
                                             
                                             StepVec.push_back(0);
                                             
                                         for(int k = 1; k<tVec1.size(); k++){
                                             DistVec.push_back((std::pow((tVec1[k][0] - tVec2[k][0]), 2) /2) + (std::pow((tVec1[k][1]-tVec2[k][1]),2) /2));
                                             
                                             PhaseVec.push_back(std::pow((tVec1[k][0]-tVec2[k][0] - DiffMean), 2)  +  std::pow((tVec1[k][1]-tVec2[k][1] - DiffMean), 2));
                                             
                                             StepVec.push_back((std::pow((tVec1[k][0]-tVec1[k-1][1]) - (tVec2[k][0]-tVec2[k-1][1]),2)/2)  + (std::pow((tVec1[k][1]-tVec1[k][0]) - (tVec2[k][1]-tVec2[k][0]), 2)/2));
                                         }
                                         double CurrDist = std::accumulate(DistVec.begin(),DistVec.end(),0);
                                         double CurrPhase = std::accumulate(PhaseVec.begin(),PhaseVec.end(),0);
                                         double CurrStep = std::accumulate(StepVec.begin(),StepVec.end(),0);
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

                                         std::vector<std::vector<double>> tVec1;

                                         std::vector<std::vector<double>> tVec2;

                                             std::vector<double> DistVec;
                                             std::vector<double> PhaseVec;
                                                 for(int m = 0; m<CSet1[0].size(); m++){
                                                     std::vector<double> TPair1;
                                                     TPair1.push_back(TSeqArr1[CSet1[i][m]]);
                                                     TPair1.push_back(TSeqDep1[CSet1[i][m]]);
                                                     tVec1.push_back(TPair1);
                                                     std::vector<double> TPair2;
                                                     TPair2.push_back(TSeqArr2[CSet2[j][m]]);
                                                     TPair2.push_back(TSeqDep2[CSet2[j][m]]);
                                                     tVec2.push_back(TPair2);
                                                 }
                                             
                                             std::vector<double> DiffVec;
                                             double DiffMean;
                                             for(int k = 0; k<tVec1.size(); k++){
                                                 DiffVec.push_back((tVec1[k][0] - tVec2[k][0]) + (tVec1[k][1] - tVec2[k][1]));
                                             }
                                             
                                             double DiffSum = std::accumulate(DiffVec.begin(), DiffVec.end(), 0LL);
                                             DiffMean = DiffSum / (2 * DiffVec.size());
                                             
                                             
                                         for(int k = 0; k<tVec1.size(); k++){
                                             DistVec.push_back((std::pow((tVec1[k][0] - tVec2[k][0]), 2) /2) + (std::pow((tVec1[k][1]-tVec2[k][1]),2) /2));
                                             
                                             PhaseVec.push_back(std::pow(tVec1[k][0]-tVec2[k][0] - DiffMean, 2)  +  std::pow(tVec1[k][1]-tVec2[k][1] - DiffMean, 2));
                                         }
                                         double CurrDist = std::accumulate(DistVec.begin(),DistVec.end(),0);
                                         double CurrPhase = std::accumulate(PhaseVec.begin(),PhaseVec.end(),0);
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

                                     std::vector<std::vector<double>> tVec1;

                                     std::vector<std::vector<double>> tVec2;

                                         std::vector<double> DistVec;
                                         std::vector<double> PhaseVec;
                                         std::vector<double> StepVec;
                                             for(int m = 0; m<CSet1[0].size(); m++){
                                                 std::vector<double> TPair1;
                                                 TPair1.push_back(TSeqArr1[CSet1[i][m]]);
                                                 TPair1.push_back(TSeqDep1[CSet1[i][m]]);
                                                 tVec1.push_back(TPair1);
                                                 std::vector<double> TPair2;
                                                 TPair2.push_back(TSeqArr2[CSet2[j][m]]);
                                                 TPair2.push_back(TSeqDep2[CSet2[j][m]]);
                                                 tVec2.push_back(TPair2);
                                             }
                                         
                                         DistVec.push_back((std::pow((tVec1[0][0] - tVec2[0][0]), 2) /2) + (std::pow((tVec1[0][1]-tVec2[0][1]),2) /2));
                                                                                  
                                         StepVec.push_back(0);
                                         
                                     for(int k = 1; k<tVec1.size(); k++){
                                         DistVec.push_back((std::pow((tVec1[k][0] - tVec2[k][0]), 2) /2) + (std::pow((tVec1[k][1]-tVec2[k][1]),2) /2));
                                         
                                         StepVec.push_back((std::pow((tVec1[k][0]-tVec1[k-1][1]) - (tVec2[k][0]-tVec2[k-1][1]),2)/2)  + (std::pow((tVec1[k][1]-tVec1[k][0]) - (tVec2[k][1]-tVec2[k][0]), 2)/2));
                                     }
                                     double CurrDist = std::accumulate(DistVec.begin(),DistVec.end(),0);
                                     double CurrStep = std::accumulate(StepVec.begin(),StepVec.end(),0);
                                     
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

                                     std::vector<std::vector<double>> tVec1;

                                     std::vector<std::vector<double>> tVec2;

                                         std::vector<double> DistVec;

                                             for(int m = 0; m<CSet1[0].size(); m++){
                                                 std::vector<double> TPair1;
                                                 TPair1.push_back(TSeqArr1[CSet1[i][m]]);
                                                 TPair1.push_back(TSeqDep1[CSet1[i][m]]);
                                                 tVec1.push_back(TPair1);
                                                 std::vector<double> TPair2;
                                                 TPair2.push_back(TSeqArr2[CSet2[j][m]]);
                                                 TPair2.push_back(TSeqDep2[CSet2[j][m]]);
                                                 tVec2.push_back(TPair2);
                                             }
                              for(int k = 0; k<tVec1.size(); k++){
                                  DistVec.push_back((std::pow((tVec1[k][0] - tVec2[k][0]), 2) /2) + (std::pow((tVec1[k][1]-tVec2[k][1]),2) /2));
                              }
                              double CurrDist = std::accumulate(DistVec.begin(),DistVec.end(),0);
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

                                     std::vector<std::vector<double>> tVec1;

                                     std::vector<std::vector<double>> tVec2;

                                         std::vector<double> PhaseVec;
                                         std::vector<double> StepVec;
                                             for(int m = 0; m<CSet1[0].size(); m++){
                                                 std::vector<double> TPair1;
                                                 TPair1.push_back(TSeqArr1[CSet1[i][m]]);
                                                 TPair1.push_back(TSeqDep1[CSet1[i][m]]);
                                                 tVec1.push_back(TPair1);
                                                 std::vector<double> TPair2;
                                                 TPair2.push_back(TSeqArr2[CSet2[j][m]]);
                                                 TPair2.push_back(TSeqDep2[CSet2[j][m]]);
                                                 tVec2.push_back(TPair2);
                                             }
                                         
                                         std::vector<double> DiffVec;
                                         double DiffMean;
                                         for(int k = 0; k<tVec1.size(); k++){
                                             DiffVec.push_back((tVec1[k][0] - tVec2[k][0]) + (tVec1[k][1] - tVec2[k][1]));
                                         }
                                         
                                         double DiffSum = std::accumulate(DiffVec.begin(), DiffVec.end(), 0LL);
                                         DiffMean = DiffSum / (2 * DiffVec.size());
                                         
                                         PhaseVec.push_back(std::pow(tVec1[0][0]-tVec2[0][0] - DiffMean, 2)  +  std::pow(tVec1[0][1]-tVec2[0][1] - DiffMean, 2));
                                         
                                         StepVec.push_back(0);
                                         
                                     for(int k = 1; k<tVec1.size(); k++){
                                         PhaseVec.push_back(std::pow(tVec1[k][0]-tVec2[k][0] - DiffMean, 2)  +  std::pow(tVec1[k][1]-tVec2[k][1] - DiffMean, 2));
                                         
                                         StepVec.push_back((std::pow((tVec1[k][0]-tVec1[k-1][1]) - (tVec2[k][0]-tVec2[k-1][1]),2)/2)  + (std::pow((tVec1[k][1]-tVec1[k][0]) - (tVec2[k][1]-tVec2[k][0]), 2)/2));
                                     }
                                     double CurrPhase = std::accumulate(PhaseVec.begin(),PhaseVec.end(),0);
                                     double CurrStep = std::accumulate(StepVec.begin(),StepVec.end(),0);
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

                                     std::vector<std::vector<double>> tVec1;

                                     std::vector<std::vector<double>> tVec2;

                                         std::vector<double> PhaseVec;
                                             for(int m = 0; m<CSet1[0].size(); m++){
                                                 std::vector<double> TPair1;
                                                 TPair1.push_back(TSeqArr1[CSet1[i][m]]);
                                                 TPair1.push_back(TSeqDep1[CSet1[i][m]]);
                                                 tVec1.push_back(TPair1);
                                                 std::vector<double> TPair2;
                                                 TPair2.push_back(TSeqArr2[CSet2[j][m]]);
                                                 TPair2.push_back(TSeqDep2[CSet2[j][m]]);
                                                 tVec2.push_back(TPair2);
                                             }
                                         
                                         std::vector<double> DiffVec;
                                         double DiffMean;
                                         for(int k = 0; k<tVec1.size(); k++){
                                             DiffVec.push_back((tVec1[k][0] - tVec2[k][0]) + (tVec1[k][1] - tVec2[k][1]));
                                         }
                                         
                                         double DiffSum = std::accumulate(DiffVec.begin(), DiffVec.end(), 0);
                                         DiffMean = DiffSum / (2 * DiffVec.size());
                                         
                                     for(int k = 0; k<tVec1.size(); k++){
                                         PhaseVec.push_back(std::pow((tVec1[k][0]-tVec2[k][0] - DiffMean), 2)  +  std::pow((tVec1[k][1]-tVec2[k][1] - DiffMean), 2));
                                         
                                     }
                                     double CurrPhase = std::accumulate(PhaseVec.begin(),PhaseVec.end(),0);
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

                                 std::vector<std::vector<double>> tVec1;

                                 std::vector<std::vector<double>> tVec2;


                                     std::vector<double> StepVec;
                                         for(int m = 0; m<CSet1[0].size(); m++){
                                             std::vector<double> TPair1;
                                             TPair1.push_back(TSeqArr1[CSet1[i][m]]);
                                             TPair1.push_back(TSeqDep1[CSet1[i][m]]);
                                             tVec1.push_back(TPair1);
                                             std::vector<double> TPair2;
                                             TPair2.push_back(TSeqArr2[CSet2[j][m]]);
                                             TPair2.push_back(TSeqDep2[CSet2[j][m]]);
                                             tVec2.push_back(TPair2);
                                         }
                                     
                                     StepVec.push_back(0);
                                     
                                 for(int k = 1; k<tVec1.size(); k++){
                                     StepVec.push_back((std::pow((tVec1[k][0]-tVec1[k-1][1]) - (tVec2[k][0]-tVec2[k-1][1]),2)/2)  + (std::pow((tVec1[k][1]-tVec1[k][0]) - (tVec2[k][1]-tVec2[k][0]), 2)/2));
                                 }
                                 double CurrStep = std::accumulate(StepVec.begin(),StepVec.end(),0);
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

                      std::vector<std::vector<std::vector<std::vector<double>>>> BestPair(l);
                     for(int f = 0; f < l; f++){
                         // We'd need to build this specific case of Constr Table a lil differently, but it's manageable!

                         
                             std::vector<std::vector<double>> TPairVec1;
                             
                             for(int j = 0; j<CSet1[0].size(); j++){
                                 std::vector<double> TPair1;
                                 TPair1.push_back(TSeqArr1[CSet1[BestConsts[f][0]][j]]);
                                 TPair1.push_back(TSeqDep1[CSet1[BestConsts[f][0]][j]]);
                                 TPairVec1.push_back(TPair1);
                             }
                            
                         
                         
                         
                             std::vector<std::vector<double>> TPairVec2;
                             
                             for(int j = 0; j<CSet2[0].size(); j++){
                                 std::vector<double> TPair2;
                                 TPair2.push_back(TSeqArr2[CSet2[BestConsts[f][1]][j]]);
                                 TPair2.push_back(TSeqDep2[CSet2[BestConsts[f][1]][j]]);
                                 TPairVec2.push_back(TPair2);
                             }
                             
                         
                      BestPair[f].push_back(TPairVec1);
                      BestPair[f].push_back(TPairVec2);
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
    
  std::cout << Size1 << " is the length of the first object.\n";
  std::cout << Size2 << " is the length of the second object.\n";
    
    std::vector<std::vector<int>> DPmat;
    
    DPmatInit(Size1, Size2, DPmat);
    
    // Preliminary setting/context for running the process
    
    
    // Find length of LCS


        int lcslen = lcscore(Fish1Seq, Fish2Seq, Size1, Size2, 0, 0, DPmat);
    

    std::cout << lcslen << " is the length of the lcs.\n";
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
    LCSConstructHunt(LCSset, Fish1Seq, Fish2Seq, PullSet, TSeqArr1, TSeqDep1, TSeqArr2, TSeqDep2, Methods, LocLib, lcslen);
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
