#include<iostream>

using namespace std;

int fibonacci(int n){
    if (n < 2) {
        return n;
    } else {
        return fibonacci(n-1) + fibonacci(n-2);
    }
}

int main(){
    int n;
    cout << "Enter n: ";
    cin >> n;
    cout << "The fibonacci of "<<n<<" is "<<fibonacci(n)<<endl;
    return 0;
}