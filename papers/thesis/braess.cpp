/*
 * "Two roads diverged in a city, and I--
 * I took the one less traveled by because it had a lower toll,
 * And that has made all the difference to the overall travel time."
 */

#include <iostream>
#include <limits>

using namespace std;

// Arguments are the number of drivers taking each possible route.
double compute_cost(int n1, int n2, int n3) {
  // How many are taking the upper and lower path?
  int u = n1 + n2;
  int d = n3;

  // The cost of taking each route
  double c1 = (u / 10.0) + 4.50;
  double c2 = (u / 10.0) + 0 + (d / 10.0);
  double c3 = 4.50 + (d / 10.0);

  return (n1 * c1) + (n2 * c2) + (n3 * c3);
}

void describe(int n1, int n2, int n3) {
  cout << "  cost(n1 = " << n1 << ", n2 = " << n2 << ", n3 = " << n3 << ") = "
       << compute_cost(n1, n2, n3) << endl;
}

// Try every combination of drivers along the 3 routes.
void brute_force(int N) {
  double min = numeric_limits<double>::max();
  int min_n1, min_n2, min_n3;

  for (int n1 = 0; n1 <= N; ++n1) {
    for (int n2 = 0; n2 <= N - n1; ++n2) {
      int n3 = N - n1 - n2;
      double cost = compute_cost(n1, n2, n3);
      if (cost < min) {
        min = cost;
        min_n1 = n1;
        min_n2 = n2;
        min_n3 = n3;
      }
    }
  }

  cout << "Optimal:\n";
  describe(min_n1, min_n2, min_n3);

  cout << "Without the shortcut:\n";
  describe(N / 2, 0, N / 2);

  cout << "If everyone is greedy:\n";
  describe(0, N, 0);
}

int main () {
  brute_force(10000);
  return 0;
}
