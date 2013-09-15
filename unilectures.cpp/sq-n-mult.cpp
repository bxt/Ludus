#include <iostream>
#include <ostream>

int main()
{
	   std::cout << "Square and Multiply" << std::endl;
	   int base = 2;
	   int exponent = 5;
	   int modulus = 2247;

	   int res = 1;
	   while (exponent != 0) {
	   	while(exponent % 2 == 0) {
			exponent = exponent/2;
			res = (res * res) % modulus;
		}
		exponent = exponent - 1;
		res = (res * base) % modulus;
	   }
	   std::cout << res << std::endl;
}

