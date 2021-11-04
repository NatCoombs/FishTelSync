//
//  LCSCpp.cpp
//
//
//  Created by Nat Coombs on 10/25/21.
//

#include <stdio.h>
#include <iostream>
#include <string.h>
#include <vector>
#include <cstring>

int lcslen = 0;

int Size1;
int Size2;

int MaxSize;


std::vector<std::vector<int>> DPmat(1000,std::vector<int>(1000,-1));

//int k = 0;

std::vector<std::string> data;
std::vector<std::string> F1Index;
std::vector<std::string> F2Index;
int StringIndex = 0;
std::vector<std::string> Fish1Seq;
std::vector<std::string> Fish2Seq;
std::vector<std::vector<std::string>> LCSset;
std::vector<std::vector<std::vector<std::string>>> PullSet;
std::vector<std::vector<int>> FMT1;
std::vector<std::vector<int>> FMT2;
std::vector<int> Const1;
std::vector<int> Const2;
std::vector<int> Const1_;
std::vector<int> Const2_;
std::vector<std::vector<int>> CSet1;
std::vector<std::vector<int>> CSet2;
std::vector<std::vector<std::string>> PairedConstr;


void StringVecBuilder(char** argv, int Start, const char* BreakVal, const char* StopVal, int StringIndex, std::vector<std::string> &Fish1Seq, std::vector<std::string> &Fish2Seq){
    if(StringIndex == 0){
        if(std::strcmp(argv[Start],BreakVal)==0){
        StringIndex = StringIndex + 1;
        StringVecBuilder(argv, Start + 1, "Switch", "Stop", StringIndex, Fish1Seq, Fish2Seq);
        return;
    } else{
        Fish1Seq.push_back(argv[Start]);
        StringVecBuilder(argv, Start + 1, "Switch", "Stop", StringIndex, Fish1Seq, Fish2Seq);
        return;
    }
    } else{
        if(std::strcmp(argv[Start],StopVal)==0){
        return;
    } else{
        Fish2Seq.push_back(argv[Start]);
        StringVecBuilder(argv, Start + 1, "Switch", "Stop", StringIndex, Fish1Seq, Fish2Seq);
        return;
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
int lcscore(std::vector<std::string> Fish1Seq, std::vector<std::string> Fish2Seq, int Size1, int Size2, int i, int j, std::vector<std::vector<int>> & DPmat){
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

void PullAll(std::vector<std::string> Fish1Seq, std::vector<std::string> Fish2Seq, int Size1, int Size2, std::vector<std::string> & data, int indx1, int indx2, int currlcs,std::vector<std::vector<std::string>> & LCSset/* std::vector<std::vector<std::vector<std::string>>> & PullSet, std::vector<std::string> &F1Index, std::vector<std::string> &F2Index*/){

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
                                                      //Need to figure out how to pass ALL constructors
                                                      //Store the LCS and constructors to see how it goes.
                                                    data[currlcs] = Fish1Seq[i];
                                                      //F1Index[currlcs] = std::to_string(i + 1);
                                                      //F2Index[currlcs] = std::to_string(j + 1);
                                                    PullAll(Fish1Seq, Fish2Seq, Size1, Size2, data, i+1, j+1, currlcs+1, LCSset/* PullSet, F1Index, F2Index*/);
                                                    break;
                                                  }
                                  
                              }
            }
}
void LCSConstructStepper(std::vector<std::string> LCS, std::vector<std::vector<int>> FishMatchTable, std::vector<std::vector<int>> & ConstrSet, std::vector<int> & ConstrVec, int RunDex, int currlcs){
    if(currlcs == lcslen){
        ConstrSet.push_back(ConstrVec);
        return;
    }
    // Remember that this is inside a for loop using i, but it shouldn't cause an issue to loop like this anyway
    for(int j = 0; j<FishMatchTable[RunDex].size(); j++){
        ConstrVec[RunDex] = FishMatchTable[RunDex][j];
        if(RunDex == 0 || ConstrVec[RunDex] > ConstrVec[RunDex-1]){
            LCSConstructStepper(LCS, FishMatchTable, ConstrSet, ConstrVec, RunDex + 1, currlcs + 1);
        }
    }
    return;
}

void LCSConstructHunt(std::vector<std::vector<std::string>> LCSset, std::vector<std::string> Fish1Seq, std::vector<std::string> Fish2Seq, std::vector<std::vector<std::vector<std::string>>> & PullSet){
    for(int i = 0; i<LCSset.size(); i++){
        std::vector<std::vector<int>> FMT1(lcslen);
        std::vector<std::vector<int>> FMT2(lcslen);
        for(int j = 0; j<lcslen; j++){
            std::vector<int> Const1;
            std::vector<int> Const2;
            for(int k = 0; k<Fish1Seq.size(); k++){
                if(Fish1Seq[k].compare(LCSset[i][j])==0){
                    Const1.push_back(k);
                }}
                
                for(int k = 0; k<Fish2Seq.size(); k++){
                    if(Fish2Seq[k].compare(LCSset[i][j])==0){
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
        LCSConstructStepper(LCSset[i], FMT1, CSet1, Const1_, 0, 0);
        LCSConstructStepper(LCSset[i], FMT2, CSet2, Const2_, 0, 0);
        // LOGIC WORKS TO HERE a/o 1:15 PM 11/3

        // After repeating for each fish sequence, staple together each permutation of the LCS & constructors
        // Think serial for loop using LCSset[i][j] and our constructor sets
     //   int nPermute = CSet1.size() * CSet2.size();
        for(int c1=0; c1<CSet1.size(); c1 ++){
            for(int c2=0; c2<CSet2.size(); c2 ++){
           std::vector<std::vector<std::string>> PairedConstr;
                std::vector<std::string> CSet1Str(lcslen);
                std::vector<std::string> CSet2Str(lcslen);
                for(int j=0;j<lcslen;j++){
                    //Breaking in here
                    CSet1Str[j] = std::to_string(CSet1[c1][j]);
                    CSet2Str[j] = std::to_string(CSet2[c2][j]);
                }
//                std::vector<std::string> CSet1Str(CSet1[c1].begin(),CSet1[c1].end());
//                std::vector<std::string> CSet2Str(CSet2[c2].begin(),CSet2[c2].end());
                PairedConstr.push_back(LCSset[i]);
                PairedConstr.push_back(CSet1Str);
                PairedConstr.push_back(CSet2Str);
                
                PullSet.push_back(PairedConstr);
                
            }}}
    return;
    }


std::vector<std::vector<std::vector<std::string>>> FullLCSExtractor(std::vector<std::string> Fish1Seq, std::vector<std::string> Fish2Seq, int argc, char** argv){

   Size1 = Fish1Seq.size();
   Size2 = Fish2Seq.size();
    
  std::cout << Size1 << " is the length of the first object.\n";
  std::cout << Size2 << " is the length of the second object.\n";

    DPmatInit(Size1, Size2, DPmat);
    
    MaxSize = Size1 * Size2;

    // Preliminary setting/context for running the process
    
    
    // Find length of LCS


        lcslen = lcscore(Fish1Seq, Fish2Seq, Size1, Size2, 0, 0, DPmat);
    

    std::cout << lcslen << " is the length of the lcs.\n";
    // Use recursive backtracing to generate all LCS
    // data, F1Index, & F2Index are used for storage
    // Each should be as long as the lcs
    std::vector<std::string> data(lcslen);
    std::vector<std::string> F1Index(lcslen);
    std::vector<std::string> F2Index(lcslen);
    std::vector<std::vector<std::string>> LCSset;
    
    std::vector<std::vector<std::vector<std::string>>> PullSet; // Trying a vector of vectors of vectors of strings

    PullAll(Fish1Seq, Fish2Seq, Size1, Size2, data, 0, 0, 0, LCSset/*PullSet, F1Index, F2Index*/);
    
   // std::cout << "It got this far" << "\n";
    // std::cout << PullSet[0][0][0] << PullSet[0][0][1] << PullSet[0][0][2] << PullSet[0][0][3] << "\n";
    // std::cout << PullSet[0][1][0] << PullSet[0][1][1] << PullSet[0][1][2] << PullSet[0][1][3] << "\n";
    // std::cout << PullSet[0][2][0] << PullSet[0][2][1] << PullSet[0][2][2] << PullSet[0][2][3] << "\n";
    // LCSset now contains every LCS, need to add a utility/pair of utilities for comparing against vectors of strings to find constructors and staple combinations of {vector(LCS),vector(F1Construct),vector(F2Construct)} onto PullSet
    LCSConstructHunt(LCSset, Fish1Seq, Fish2Seq, PullSet);
    return PullSet;
}

void LCSPrinter(std::vector<std::vector<std::vector<std::string>>> LCSOut, int lcslen, char** argv, int argc){
    if(strcmp(argv[argc-1], "Debug")==0){
    for(int i = 0; i < LCSOut.size(); i++){
        for(int k = 0;k < (lcslen); k++){
            std::cout << LCSOut[i][0][k] << " ";
        }
        std::cout << "\n";
        
        for(int k = 0;k < (lcslen); k++){
            std::cout << LCSOut[i][1][k] << " ";
        }
        std::cout << "\n";
        
        for(int k = 0;k < (lcslen); k++){
            std::cout << LCSOut[i][2][k] << " ";
        }
        std::cout << "\n";
        }
    }
}

int main(int argc, char** argv){
    StringVecBuilder(argv, 1, "Switch", "Stop", StringIndex, Fish1Seq, Fish2Seq);
    std::vector<std::vector<std::vector<std::string>>> LCSOut;
    LCSOut = FullLCSExtractor(Fish1Seq, Fish2Seq, argc, argv);
    LCSPrinter(LCSOut,lcslen,argv,argc);
    return 0;
}
