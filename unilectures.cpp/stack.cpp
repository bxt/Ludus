#include <iostream>
#include <ostream> // noetig?

const int MAX = 3;

class Stack {
public:
	Stack();
	bool push(int top);
	int pop();
	bool empty() const;
	int peek() const;
	int const * begin() const;
	int const * end() const;

private:
	int data_[MAX];
	int size_;
};

Stack::Stack(): size_(0)  {}

bool Stack::push(int top) {
	if (size_ >= MAX) return false;
	data_[size_] = top;
	size_++;
	return true;
}

int Stack::pop() {
	if(empty()) return -1;
	return data_[--size_];
}

bool Stack::empty() const {
	return size_ == 0;
}

int Stack::peek() const {
	return data_[size_-1];
}

int const * Stack::begin() const {
	return data_;
}

int const * Stack::end() const {
	return data_+size_;
}

int main()
{
	   std::cout << "Der tolle Stack-Test" << std::endl;
	   Stack s;
	   std::cout << "Leer?" << std::endl;
	   std::cout << s.empty() << std::endl;
	   std::cout << "Pushen" << std::endl;
	   std::cout << s.push(4) << std::endl;
	   std::cout << "Leer?" << std::endl;
	   std::cout << s.empty() << std::endl;
	   std::cout << "3xPushen" << std::endl;
	   std::cout << s.push(3) << std::endl;
	   std::cout << s.push(5) << std::endl;
	   std::cout << s.push(6) << std::endl;
	   std::cout << "Peeken:" << std::endl;
	   std::cout << s.peek() << std::endl;
	   std::cout << "Leer?" << std::endl;
	   std::cout << s.empty() << std::endl;
	   std::cout << "Iterieren" << std::endl;
	   for (int const * i = s.begin(); i < s.end(); i++) {
	   	std::cout << *i << std::endl;
	   }
	   std::cout << "3xPoppen" << std::endl;
	   std::cout << s.pop() << std::endl;
	   std::cout << s.pop() << std::endl;
	   std::cout << s.pop() << std::endl;
	   std::cout << "Leer?" << std::endl;
	   std::cout << s.empty() << std::endl;
	   std::cout << "Ueberpoppen" << std::endl;
	   std::cout << s.pop() << std::endl;
	   std::cout << "Push+Pop" << std::endl;
	   std::cout << s.push(42) << std::endl;
	   std::cout << s.pop() << std::endl;
	   std::cout << "Leer?" << std::endl;
	   std::cout << s.empty() << std::endl;
}

