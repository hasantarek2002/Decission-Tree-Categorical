#include <iostream>
#include <string>
#include <vector>
#include <fstream>
#include <map>
#include <math.h>
#include <float.h>
#include <cstdlib>
#include <iomanip>

using namespace std;

typedef vector<string> vs;
typedef vector<vs> vvs;
typedef vector<int> vi;
typedef map<string, int> msi;
typedef vector<double> vd;

struct node													// struct node defines the structure of a node of the decision tree
{
	string splitOn;											// Stores which attribute to split on at a particular node
	string label;											// Stores the class label for leaf nodes. For nodes that are not leaf nodes, it stores the value of the attribute of the parent's' split
	bool isLeaf;
	vector<string> childrenValues;
	vector<node*> children;
};

void parse(string&, vvs&);
void printAttributeTable(vvs&);
vvs pruneTable(vvs&, string&, string);
node* buildDecisionTree(vvs&, node*, vvs&);
bool isHomogeneous(vvs&);
vi countDistinct(vvs&, int);
string decideSplittingColumn(vvs&);
int returnColumnIndex(string&, vvs&);
bool tableIsEmpty(vvs&);
void printDecisionTree(node*);
string testDataOnDecisionTree(vs&, node*, vvs&, string);
int returnIndexOfVector(vs&, string);
double printPredictionsAndCalculateAccuracy(vs&, vs&);
vvs generateTableInfo(vvs &dataTable);
string returnMostFrequentClass(vvs &dataTable);





void parse(string& someString, vvs &attributeTable)
{
	int attributeCount = 0;
	vs vectorOfStrings;
	while (someString.length() != 0 && someString.find(',') != string::npos)
	{
		size_t pos;
		string singleAttribute;
		pos = someString.find_first_of(',');
		singleAttribute = someString.substr(0, pos);
		vectorOfStrings.push_back(singleAttribute);
		someString.erase(0, pos+1);
	}
	vectorOfStrings.push_back(someString);
	attributeTable.push_back(vectorOfStrings);
	vectorOfStrings.clear();
}


void printAttributeTable(vvs &attributeTable)
{
	int inner, outer;
	for (outer = 0; outer < attributeTable.size(); outer++) {
		for (inner = 0; inner < attributeTable[outer].size(); inner++) {
			cout << attributeTable[outer][inner] << "\t";
		}
		cout << endl;
	}
}


vvs pruneTable(vvs &attributeTable, string &colName, string value)
{
	int iii, jjj;
	vvs prunedTable;
	int column = -1;
	vs headerRow;
	for (iii = 0; iii < attributeTable[0].size(); iii++) {
		if (attributeTable[0][iii] == colName) {
			column = iii;
			break;
		}
	}
	for (iii = 0; iii < attributeTable[0].size(); iii++) {
		 if (iii != column) {
		 	headerRow.push_back(attributeTable[0][iii]);
		 }
	}
	prunedTable.push_back(headerRow);
	for (iii = 0; iii < attributeTable.size(); iii++) {
		vs auxRow;
		if (attributeTable[iii][column] == value) {
			for (jjj = 0; jjj < attributeTable[iii].size(); jjj++) {
				if(jjj != column) {
					auxRow.push_back(attributeTable[iii][jjj]);
				}
			}
			prunedTable.push_back(auxRow);
		}
	}
	return prunedTable;
}


node* buildDecisionTree(vvs &table, node* nodePtr, vvs &tableInfo)
{
	if (tableIsEmpty(table)) {
		return NULL;
	}
	if (isHomogeneous(table)) {
		nodePtr->isLeaf = true;
		nodePtr->label = table[1][table[1].size()-1];
		return nodePtr;
	} else {
		string splittingCol = decideSplittingColumn(table);
		nodePtr->splitOn = splittingCol;
		int colIndex = returnColumnIndex(splittingCol, tableInfo);
		int iii;
		for (iii = 1; iii < tableInfo[colIndex].size(); iii++) {
			node* newNode = (node*) new node;
			newNode->label = tableInfo[colIndex][iii];
			nodePtr->childrenValues.push_back(tableInfo[colIndex][iii]);
			newNode->isLeaf = false;
			newNode->splitOn = splittingCol;
			vvs auxTable = pruneTable(table, splittingCol, tableInfo[colIndex][iii]);
			nodePtr->children.push_back(buildDecisionTree(auxTable, newNode, tableInfo));
		}
	}
	return nodePtr;
}


bool isHomogeneous(vvs &table)
{
	int iii;
	int lastCol = table[0].size() - 1;
	string firstValue = table[1][lastCol];
	for (iii = 1; iii < table.size(); iii++) {
		if (firstValue != table[iii][lastCol]) {
			return false;
		}
	}
	return true;
}


vi countDistinct(vvs &table, int column)
{
	vs vectorOfStrings;
	vi counts;
	bool found = false;
	int foundIndex;
	for (int iii = 1; iii < table.size(); iii++) {
		for (int jjj = 0; jjj < vectorOfStrings.size(); jjj++) {
			if (vectorOfStrings[jjj] == table[iii][column]) {
				found = true;
				foundIndex = jjj;
				break;
			} else {
				found = false;
			}
		}
		if (!found) {
			counts.push_back(1);
			vectorOfStrings.push_back(table[iii][column]);
		} else {
			counts[foundIndex]++;
		}
	}
	int sum = 0;
	for (int iii = 0; iii < counts.size(); iii++) {
		sum += counts[iii];
	}
	counts.push_back(sum);
	return counts;
}


string decideSplittingColumn(vvs &table)
{
	int column, iii;
	double minEntropy = DBL_MAX;
	int splittingColumn = 0;
	vi entropies;
	for (column = 0; column < table[0].size() - 1; column++) {
		string colName = table[0][column];
		msi tempMap;
		vi counts = countDistinct(table, column);       //stores which attributes how many distinct value
		vd attributeEntropy;
		double columnEntropy = 0.0;
		for (iii = 1; iii < table.size()-1; iii++) {
			double entropy = 0.0;
			if (tempMap.find(table[iii][column]) != tempMap.end()) {
				tempMap[table[iii][column]]++;
			} else {
				tempMap[table[iii][column]] = 1;
				vvs tempTable = pruneTable(table, colName, table[iii][column]);
				vi classCounts = countDistinct(tempTable, tempTable[0].size()-1);
				int jjj, kkk;
				for (jjj = 0; jjj < classCounts.size(); jjj++) {
					double temp = (double) classCounts[jjj];
					entropy -= (temp/classCounts[classCounts.size()-1])*(log(temp/classCounts[classCounts.size()-1]) / log(2));
				}
				attributeEntropy.push_back(entropy);
				entropy = 0.0;
			}
		}
		for (iii = 0; iii < counts.size() - 1; iii++) {
			columnEntropy += ((double) counts[iii] * (double) attributeEntropy[iii]);
		}
		columnEntropy = columnEntropy / ((double) counts[counts.size() - 1]);
		if (columnEntropy <= minEntropy) {
			minEntropy = columnEntropy;
			splittingColumn = column;
		}
	}
	return table[0][splittingColumn];
}


int returnColumnIndex(string &columnName, vvs &tableInfo)
{
	int iii;
	for (iii = 0; iii < tableInfo.size(); iii++) {
		if (tableInfo[iii][0] == columnName) {
			return iii;
		}
	}
	return -1;
}



bool tableIsEmpty(vvs &table)
{
	return (table.size() == 1);
}



void printDecisionTree(node* nodePtr)
{
	if(nodePtr == NULL) {
		return;
	}
	if (!nodePtr->children.empty()) {
		cout << " Value: " << nodePtr->label << endl;
		cout << "Split on: " << nodePtr->splitOn;
		int iii;
		for (iii = 0; iii < nodePtr->children.size(); iii++) {
			cout << "\t";
			printDecisionTree(nodePtr->children[iii]);
		}
		return;
        } else {
		cout << "Predicted class = " << nodePtr->label;
		return;
	}
}


string testDataOnDecisionTree(vs &singleLine, node* nodePtr, vvs &tableInfo, string defaultClass)
{
	string prediction;
	while (!nodePtr->isLeaf && !nodePtr->children.empty()) {
		int index = returnColumnIndex(nodePtr->splitOn, tableInfo);
		string value = singleLine[index];
		int childIndex = returnIndexOfVector(nodePtr->childrenValues, value);
		nodePtr = nodePtr->children[childIndex];
		if (nodePtr == NULL) {
			prediction = defaultClass;
			break;
		}
		prediction = nodePtr->label;
	}
	return prediction;
}


int returnIndexOfVector(vs &stringVector, string value)
{
	int iii;
	for (iii = 0; iii < stringVector.size(); iii++) {
		if (stringVector[iii] == value)	{
			return iii;
		}
	}
	return -1;
}

void predictionsAndCalculateAccuracy(vs &givenData, vs &predictions,vd &acc,vd &prec,vd &rec, vd &fSc)
{
    int tp=0,tn=0,fp=0,fn=0;
    double accuracy,precision,recall,fScore;
	for (int iii = 0; iii < givenData.size(); iii++) {
		/*
		if (givenData[iii] == predictions[iii]) {
			//correct++;
			outputFile << "  ------------  ";
		} else {
			outputFile << "  xxxxxxxxxxxx  ";
		}
		*/
		if(givenData[iii]=="positive" && predictions[iii]=="positive") tp++;
		else if(givenData[iii]=="negative" && predictions[iii]=="positive") fp++;
		else if(givenData[iii]=="negative" && predictions[iii]=="negative") fn++;
		else tn++;
	}
	//cout << "TP  "<<tp<<"  FP   "<<fp<<"   FN  "<<fn<<"  TN  "<<tn<<endl;
	accuracy=(double) (tp + tn) / (tp + tn + fp + fn);
	precision = (double) (tp) / (tp + fp);
	recall = (double) (tp) / (tp + fn);
	fScore = (double) (2 * recall * precision) / (recall + precision);
	/*
	cout<< "accuracy is   " << accuracy<<endl;
	cout<< "precission is   " << precision<<endl;
	cout<< "recall is   " << recall<<endl;
	cout<< "F Score is   " << fScore<<endl;
    */
	acc.push_back(accuracy);
	prec.push_back(precision);
	rec.push_back(recall);
	fSc.push_back(fScore);
}


vvs generateTableInfo(vvs &dataTable)
{
	vvs tableInfo;
	for (int iii = 0; iii < dataTable[0].size(); iii++) {
		vs tempInfo;
		msi tempMap;
		for (int jjj = 0; jjj < dataTable.size(); jjj++) {
			if (tempMap.count(dataTable[jjj][iii]) == 0) {
				tempMap[dataTable[jjj][iii]] = 1;
				tempInfo.push_back(dataTable[jjj][iii]);
			} else	{
				tempMap[dataTable[jjj][iii]]++;
			}
		}
		tableInfo.push_back(tempInfo);
	}
	return tableInfo;
}


string returnMostFrequentClass(vvs &dataTable)
{
	msi trainingClasses;
	for (int iii = 1; iii < dataTable.size(); iii++) {
		if (trainingClasses.count(dataTable[iii][dataTable[0].size()-1]) == 0) {
			trainingClasses[dataTable[iii][dataTable[0].size()-1]] = 1;
		} else {
			trainingClasses[dataTable[iii][dataTable[0].size()-1]]++;
		}
	}
	msi::iterator mapIter;
	int highestClassCount = 0;
	string mostFrequentClass;
	for (mapIter = trainingClasses.begin(); mapIter != trainingClasses.end(); mapIter++) {
		if (mapIter->second >= highestClassCount) {
			highestClassCount = mapIter->second;
			mostFrequentClass = mapIter->first;
		}
	}
	return mostFrequentClass;
}


int main(int argc, const char *argv[])
{
	ifstream inputFile;
	string singleInstance;
	vvs dataTable;
    inputFile.open("tic-tac-toe.data");
	if (!inputFile)
	{
		cerr << "Error: Training data file not found!" << endl;
		exit(-1);
	}

	while (getline(inputFile, singleInstance))
	{
		parse(singleInstance, dataTable);
	}
	inputFile.close();
	inputFile.clear();



	map<vs,bool>line;
	for(int i=0; i<dataTable.size();i++){
        vs temp=dataTable[i];
        line.insert( pair<vs,bool>(temp,false) );
	}

    vvs positiveTable,negativeTable;
    for(int i=1;i<dataTable.size();i++){
        vs temp=dataTable[i];
        if(temp[temp.size()-1]=="positive"){
            positiveTable.push_back(temp);
        }else{
            negativeTable.push_back(temp);
        }
    }

    cout<<"positive class Data : "<<positiveTable.size()<<endl;
    cout<<"negative class Data : "<<negativeTable.size()<<endl;

    ////////////////////////////////////////

    vd accuracy;
    vd precision;
    vd recall;
    vd fScore;

    for(int x=0; x<10; x++){
        cout <<endl<< "TEST  No : "<<x+1<<endl<<endl<<endl;
        int positiveTrainData,positiveTestData,negativeTrainData,negativeTestData;
        positiveTrainData=positiveTable.size()*90;
        positiveTrainData/=100;
        negativeTrainData=negativeTable.size()*90;
        negativeTrainData/=100;
        positiveTestData=positiveTable.size()-positiveTrainData;
        negativeTestData=negativeTable.size()-negativeTrainData;

        vvs trainTable,testTable;
        trainTable.push_back(dataTable[0]);
        testTable.push_back(dataTable[0]);

        //Decision tree training phase

        for(int i=0; i<positiveTrainData;i++){
            int index=rand() % positiveTrainData;
            vs temp=positiveTable[index];

            map<vs,bool>::iterator it;
            it = line.find(temp);
            if (it != line.end()){
                trainTable.push_back(temp);
                it->second=true;
            }
        }

        for(int i=0; i<negativeTrainData;i++){
            int index=rand() % negativeTrainData;
            vs temp=negativeTable[index];
            map<vs,bool>::iterator it;
            it = line.find(temp);
            if (it != line.end()){
                trainTable.push_back(temp);
                it->second=true;
            }
        }



        vvs tableInfo = generateTableInfo(trainTable);
        node* root = new node;
        root = buildDecisionTree(trainTable, root, tableInfo);
        string defaultClass = returnMostFrequentClass(trainTable);
        trainTable.clear();

        //Decision tree testing phase

        while(1){
            int index=rand() % positiveTable.size();
            vs temp=positiveTable[index];
            map<vs,bool>::iterator it;
            it = line.find(temp);
            if (it != line.end() && it->second==false){
                testTable.push_back(temp);
                it->second=true;
                positiveTestData--;
                if(positiveTestData==0) break;
            }
        }

        while(1){
            int index=rand() % negativeTable.size();
            vs temp=negativeTable[index];

            map<vs,bool>::iterator it;
            it = line.find(temp);
            if (it != line.end() && it->second==false){
                testTable.push_back(temp);
                it->second=true;
                //c++;
                negativeTestData--;
                if(negativeTestData==0) break;
            }
        }

        vs predictedClassLabels;
        vs givenClassLabels;
        for (int iii = 1; iii < testTable.size(); iii++)
        {
            string data = testTable[iii][testTable[0].size()-1];
            givenClassLabels.push_back(data);
        }
        for (int iii = 1; iii < testTable.size(); iii++)
        {
            string someString = testDataOnDecisionTree(testTable[iii], root, tableInfo, defaultClass);
            predictedClassLabels.push_back(someString);
        }
        testTable.clear();

        predictionsAndCalculateAccuracy(givenClassLabels, predictedClassLabels,accuracy,precision,recall,fScore);

        tableInfo.clear();

        ///re initialize lines from file as false
        for(int i=0; i<dataTable.size();i++){
            vs temp=dataTable[i];
            map<vs,bool>::iterator it;
            it = line.find(temp);
            if (it != line.end()){
                it->second=false;
            }
        }

    }

    //cout << "accuracy size : "<<accuracy.size()<< "    precision size : "<<precision.size()<< "    recall size : "<<recall.size()<< "    F Score size : "<<fScore.size()<<endl;
    double accuracySum=0,precissionSum=0,recallSum=0,fScoreSum=0;
    for(int i=0; i<10; i++){
        accuracySum+=accuracy[i];
        precissionSum+=precision[i];
        recallSum+=recall[i];
        fScoreSum+=fScore[i];
    }

    cout <<endl<<"---------------------"<<endl<<endl<< "Final accuracy  : "<<accuracySum/10<< endl;
    cout<<"Final  precision  : "<<precissionSum/10<<endl;
    cout<< "Final recall : "<<recallSum/10<< endl;
    cout<<"Final F Score : "<<fScoreSum/10<<endl;

	return 0;
}




