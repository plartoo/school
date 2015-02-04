// Phyo Thiha
// September 07, 2011
//
// Test program to write and read to disk several times 
// for Assignment 1/Part I testing.
// Code snippet inspired by <http://www.cplusplus.com/doc/tutorial/files/>
//
#include <iostream>
#include <fstream>
using namespace std;

int main () {
	string line;
	int i = 0;
	ofstream write_file;
	ifstream read_file;

	while (i < 20){

	write_file.open("test_write.txt");

	for (int i=0; i < 10000; i++){
		write_file << "Wahaha icecream and drinks.\n";
	}

	read_file.open("test_write.txt");
	if (read_file.is_open()){
		while (read_file.good()){
			getline(read_file, line);
		}

	}

	write_file.close();
	read_file.close();
	sleep(5);
	i++;
	}

	return 0;
}


