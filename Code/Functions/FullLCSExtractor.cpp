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


#include <stdio.h>
//#include <RcppCommon.h>
//class LCSArray {
//public:
//LCSArray();
//// this operator enables implicit Rcpp::wrap
//operator SEXP();
//}
#include <Rcpp.h>

using namespace Rcpp;
using namespace std;

int lcslen = 0;

int Size1;
int Size2;

int MaxSize;

//int * DPmat;
std::vector<std::vector<int>>/*int*/ DPmat[1000][1000];

//int DPmat[1000][1000];

//int k = 0;

std::vector<std::string> data(1);
std::vector<std::string> F1Index(1);
std::vector<std::string> F2Index(1);

std::vector<std::vector<std::vector<std::string>>> PullSet;

// Utility from: https://stackoverflow.com/questions/15380785/converting-element-of-const-rcppcharactervector-to-stdstring

//Converts Rcpp::CharacterVectors to std vectors of strings
std::vector<std::string> ChV(Rcpp::CharacterVector f) {
  std::vector<std::string> s(f.size());
  for (int i=0; i<f.size(); i++) {
    s[i] = std::string(f[i]);
  }
  return(s);
}



// Potential approach for creating the 3-D data structure for C++: http://www.cplusplus.com/forum/general/833/
//

// Need to create a version that uses ONLY C++/C code and objects, see Dirk's post on Fibonacci algs
int lcscore(std::vector<std::string> Fish1Seq, std::vector<std::string> Fish2Seq, int Size1, int Size2, int i, int j){
    // Set the memory address to be tested and modified
    int &ret = DPmat[i][j];
    
   // Rcpp::Rcout << Fish1Seq[0] << Fish1Seq[1] << Fish1Seq[2] << Fish1Seq[3] <<  "\n";

    Rcout << DPmat[i][j] << "\n";

//   Rcpp::Rcout << DPmat[1][1] << "\n";
  //  Rcpp::Rcout << DPmat[0][0] << "\n";

   // Rcout << ret << "\n";

    // Check to see if the run is done
    if (i==Size1 || j==Size2)
    {
    //   Rcout << "This should be returned on completion" << "\n";
        return ret = 0;
    }
    
    // Check to see if LCS is already computed
   // Rcout << ret << "\n";
    if (ret != -1)
        {
        //    Rcout << "This should be returned if the table is full" << "\n";
            return ret;
        }
    //Adding a tester
    
    ret = 0;

    bool matches = Fish1Seq[i] == Fish2Seq[j];
    // if characters are same return previous + 1 else
        // max of two sequences after removing i'th and j'th
        // char one by one
        if (matches == TRUE)
        {
       //     Rcout << "This should be returned on match" << "\n";

            ret = 1 + lcscore(Fish1Seq, Fish2Seq, Size1, Size2, i+1, j+1);
        }
        else
        {
      //      Rcout << "This should be returned on unmatch" << "\n";

            ret = std::max(lcscore(Fish1Seq, Fish2Seq, Size1, Size2, i+1, j),
                        lcscore(Fish1Seq, Fish2Seq, Size1, Size2, i, j+1));
            
        }
     //  Rcout << "This should be returned unless it's done" << "\n";
        return ret;

}

void PullAll(std::vector<std::string> Fish1Seq, std::vector<std::string> Fish2Seq, int Size1, int Size2, std::vector<std::string> data, int indx1, int indx2, int currlcs){

    // if currlcs is equal to lcslen then staple it to the output
        if (currlcs == lcslen)
        {
            // Declare and clear the container for the LCS & constructor set
            std::vector<std::vector<std::string>> LCSset;
            
            // Stapling together the LCS & constructor set
            LCSset.push_back(data);
            LCSset.push_back(F1Index);
            LCSset.push_back(F2Index);
            
            // Stapling the set to the output
            PullSet.push_back(LCSset);

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
                                                      //Store the LCS and constructors to see how it goes.
                                                    data[currlcs] = Fish1Seq[i];
                                                      F1Index[currlcs] = std::to_string(i);
                                                      F2Index[currlcs] = std::to_string(j);
                                                    PullAll(Fish1Seq, Fish2Seq, Size1, Size2, data, i+1, j+1, currlcs+1);
                                                    break;
                                                  }
                                  
                              }
            }
}

std::vector<std::vector<std::vector<std::string>>> FullLCSExtractor(std::vector<std::string> Fish1Seq, std::vector<std::string> Fish2Seq){
    // Clone is used to avoid memory issues

  

   Size1 = Fish1Seq.size();
   Size2 = Fish2Seq.size();
    
  //  Rcpp::Rcout << Size1 << "\n";
    // Rcpp::Rcout << Size2 << "\n";

    
    MaxSize = Size1 * Size2;

    // Preliminary setting/context for running the process
    
    
   // std::vector<std::vector<int>>/*int*/   DPmat;
    // Doesn't need to be redeclared: it's a vector, not an array
    
    //DPmat = int[Size1+1][Size2+1];
// Set length and -1 throughout using a for loop
    
    std::vector<int> FillVec;
    FillVec.assign(Size2,-1);
    for(int k=0; k<Size1; k++){
        DPmat.push_back(FillVec);
    }

    // Find length of LCS
    // Memset is for array, not for
       // memset(DPmat, -1, sizeof(DPmat));
    
    Rcpp::Rcout << DPmat[0][0] << DPmat[1][1] << DPmat[2][2] << DPmat[4][4] << "\n";

        lcslen = lcscore(Fish1Seq, Fish2Seq, Size1, Size2, 0, 0);
    
  //   Rcpp::Rcout << lcslen << "\n";

    Rcpp::Rcout << DPmat[0][0] << DPmat[1][1] << DPmat[2][2] << DPmat[4][4] << "\n";

    // Use recursive backtracing to generate all LCS
    // data[] is used for storage
    std::vector<std::string> data(lcslen);
    std::vector<std::string> F1Index(lcslen);
    std::vector<std::string> F2Index(lcslen);
    std::vector<std::vector<std::vector<std::string>>> PullSet; // Trying a vector of vectors of vectors of strings
    

    PullAll(Fish1Seq, Fish2Seq, Size1, Size2, data, 0, 0, 0);

    return PullSet;
}


// [[Rcpp::export]]

RcppExport SEXP LCSExtract(SEXP Fish1Sequence, SEXP Fish2Sequence){
    // step 0: convert input to C++ types
    CharacterVector Fish1Seq_ = clone(as<CharacterVector>(Fish1Sequence)), Fish2Seq_ = clone(as<CharacterVector>(Fish2Sequence));

    std::vector<std::string> Fish1Seq = ChV(Fish1Seq_), Fish2Seq = ChV(Fish2Seq_);
    
  //  Rcpp::Rcout << Fish1Seq[0] << Fish1Seq[1] << Fish1Seq[2] << Fish1Seq[3] <<  "\n";

    
    // step 1: call the underlying C++ function
    std::vector<std::vector<std::vector<std::string>>> LeastComm = FullLCSExtractor(Fish1Seq, Fish2Seq);


    //    Rcout << LeastComm[1][1][1]; //<< LeastComm[0][0][1] << LeastComm[0][0][2] << "\n";
    //     Rcout << LeastComm[0][1][0] << LeastComm[0][1][1] << LeastComm[0][1][2] << "\n";
    //     Rcout << LeastComm[0][2][0] << LeastComm[0][2][1] << LeastComm[0][2][2] << "\n";


        //  Rcpp::List LCSOut = as<Rcpp::List>(LeastComm); //This conversion may not be 1:1, but I really, really hope it is.
                                                    // Spoiler: it's not.
    // step 2: return the result as a SEXP
    return wrap(LeastComm);
    // Currently returning an empty 1D list, so that's fun
}
