#include <iostream>
#include <ostream>

int main()
{
	   std::cout << "Square and Multiply" << std::endl;
	   int b = 2;
	   int exp = 1234;
	   int mod = 2237;

	   int res = 1;
	   while (exp > 0) {
		   if((exp % 2) != 0) res = (res*b) % mod;
		   b = (b*b) % mod;
		   exp = exp/2;
	   }
	   std::cout << res << std::endl;
}

