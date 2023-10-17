#include "iostream"
#include "vector"
#include "string"
#include "algorithm"
#include "fstream"
#include "boost/multiprecision/cpp_int.hpp"

using namespace std;
using namespace boost::multiprecision;
vector <cpp_int> deg2(cpp_int el, cpp_int n) {
	vector <cpp_int> res;
	while (n != 0) {
		if (n / el == 1) {
			res.push_back(el);
			n -= el;
			el = 1;
		}
		else
			el *= 2;
	}
	return res;
}


cpp_int multMod(cpp_int n, cpp_int mod, vector <pair <cpp_int, cpp_int>> lst) {
	if (lst.size() == 1) {
		cpp_int res = 1;
		for (int i = 0; i < lst[0].second; i++)
			res = res * lst[0].first % mod;
		return res;
	}
	else if (lst[0].second == 1) {
		cpp_int el = lst[0].first;
		lst.erase(lst.begin());
		return (el * multMod(n, mod, lst)) % mod;
	}
	else {
		for (int i = 0; i < lst.size(); i++)
			if (lst[i].second > 1) {
				lst[i].first = (lst[i].first * lst[i].first) % mod;
				lst[i].second /= 2;
			}
		return multMod(n, mod, lst);
	}
}


cpp_int powClosed(cpp_int x, cpp_int y, cpp_int mod) {
	if (y == 0)
		return 1;

	vector <cpp_int> lst = deg2(1, y);
	vector <pair <cpp_int, cpp_int>> xDegs;
	for (int i = 0; i < lst.size(); i++)
		xDegs.push_back(make_pair(x, lst[i]));

	cpp_int res = multMod(x, mod, xDegs);
	return res;
}


cpp_int pow(cpp_int x, cpp_int y) {
	cpp_int res = 1;
	for (int i = 0; i < y; i++)
		res *= x;
	return res;
}


cpp_int decForm(string x) {
	cpp_int res = 0, deg = 1;
	if (x.back() == '1')
		res += 1;
	for (int i = 1; i < x.length(); i++) {
		deg = deg * 2;
		if (x[x.length() - i - 1] == '1')
			res += deg;
	}
	return res;
}


string binForm(cpp_int x) {
	string bitter = "";
	while (x != 0) {
		bitter = (x % 2 == 0 ? "0" : "1") + bitter;
		x = x / 2;
	}
	if (bitter == "")
		return "0";
	return bitter;
}


bool miller_rabin(cpp_int n, int k = 10) {
	if (n == 0 || n == 1)
		return false;

	cpp_int d = n - 1;
	cpp_int s = 0;
	while (d % 2 == 0) {
		s++;
		d = d / 2;
	}

	cpp_int nDec = n - 1;
	for (int i = 0; i < k; i++) {
		cpp_int a = rand() % nDec;
		if (a == 0 || a == 1)
			a = a + 2;

		cpp_int x = powClosed(a, d, n);
		if (x == 1 || x == nDec)
			continue;

		bool flag = false;
		for (int j = 0; j < s; j++) {
			x = (x * x) % n;
			if (x == nDec) {
				flag = true;
				break;
			}
		}
		if (!flag)
			return false;
	}

	return true;
}


pair <cpp_int, cpp_int> advancedEuclid(cpp_int a, cpp_int b) {
	if (a < 0 || b < 0)
		throw string{ "Выполнение невозможно: a < 0 или b < 0" };

	cpp_int q, aPrev = a, aCur = b, aNext = -1;
	cpp_int xPrev = 1, xCur = 0, xNext;
	cpp_int yPrev = 0, yCur = 1, yNext;
	while (aNext != 0) {
		q = aPrev / aCur;
		aNext = aPrev % aCur;
		aPrev = aCur; aCur = aNext;

		xNext = xPrev - (xCur * q);
		xPrev = xCur; xCur = xNext;

		yNext = yPrev - (yCur * q);
		yPrev = yCur; yCur = yNext;
	}

	return make_pair(xPrev, yPrev);
}


cpp_int genPrimeNum(int l) {
	string pBin = "1", pBinMax = "1", pBinMin = "1";
	for (int i = 1; i <= l - 2; i++) {
		pBin += to_string(rand() % 2);
		pBinMax += "1";
		pBinMin += "0";
	}
	pBin += "1"; pBinMax += "1"; pBinMin += "1";

	cpp_int p = decForm(pBin), pMax = decForm(pBinMax), pMin = decForm(pBinMin);
	while (p % 6 != 1)
		p--;
	if (p < pMin)
		p += 6;
	while (pMin % 6 != 1)
		pMin++;

	for (p; p <= pMax; p += 6)
		if (miller_rabin(p))
			return p;
	for (pMin; pMin < p; pMin += 6)
		if (miller_rabin(pMin))
			return pMin;
}


cpp_int symbolLegendre(cpp_int a, cpp_int p) {
	if (a % p == 0)
		return 0;
	while (a < 0)
		a += p;
	cpp_int res = powClosed(a, (p - 1) / 2, p);
	return res == 1 ? 1 : -1;
}


cpp_int sqrtFromZp(cpp_int a, cpp_int p) {
	cpp_int m = 0, q = p - 1;
	while (q % 2 != 1) {
		m++;
		q /= 2;
	}

	cpp_int b = rand() % p;
	while (symbolLegendre(b, p) != -1)
		b = (b + 1) % p;

	vector <cpp_int> kArr;
	for (int i = 1;; i++) {
		cpp_int k = 0;
		while (powClosed(a, pow(2, k) * q, p) != 1)
			k++;
		kArr.push_back(k);
		if (k == 0)
			break;
		a = (a * pow(b, pow(2, m - kArr.back()))) % p;
	}

	cpp_int r = powClosed(a, (q + 1) / 2, p);
	for (int i = kArr.size() - 2; i >= 0; i--)
		r = (r * advancedEuclid(pow(b, pow(2, m - kArr[i] - 1)), p).first) % p;

	return r;
}


pair <cpp_int, cpp_int> decomposeP(cpp_int D, cpp_int p) { //P.s. Параметр D подаётся без отрицательного знака!
	if (symbolLegendre(-D, p) == -1) {
		return make_pair(0, 0);
	}

	cpp_int u = sqrtFromZp(-D + p, p);
	for (int j = 1; (u * u) % p - p != -3; j++)
		u = sqrtFromZp(-D + p, p);

	int i = 0;
	vector <cpp_int> us{ u };
	vector <cpp_int> ms{ p };
	for (i;; i++) {
		ms.push_back((us[i] * us[i] + D) / ms[i]);
		us.push_back(min(us[i] % ms[i + 1], ms[i + 1] - us[i] % ms[i + 1]));
		if (ms[i + 1] == 1)
			break;
	}

	vector <cpp_int> as(i + 1, us[i]);
	vector <cpp_int> bs(i + 1, 1);
	for (i; i != 0; i--) {
		if ((us[i - 1] * as[i] + D * bs[i]) % (as[i] * as[i] + D * bs[i] * bs[i]) == 0)
			as[i - 1] = (us[i - 1] * as[i] + D * bs[i]) / (as[i] * as[i] + D * bs[i] * bs[i]);
		else
			as[i - 1] = (-us[i - 1] * as[i] + D * bs[i]) / (as[i] * as[i] + D * bs[i] * bs[i]);

		if ((-as[i] + us[i - 1] * bs[i]) % (as[i] * as[i] + D * bs[i] * bs[i]) == 0)
			bs[i - 1] = (-as[i] + us[i - 1] * bs[i]) / (as[i] * as[i] + D * bs[i] * bs[i]);
		else
			bs[i - 1] = (-as[i] - us[i - 1] * bs[i]) / (as[i] * as[i] + D * bs[i] * bs[i]);
	}

	return make_pair(as[0], bs[0]);
}


cpp_int cubeDecuct(cpp_int a, cpp_int p) { //Кубический вычет
	if (a % p == 0)
		return 0;
	while (a < 0)
		a += p;
	cpp_int res = powClosed(a, (p - 1) / 3, p);
	return res == 1 ? 1 : -1;
}


pair <cpp_int, cpp_int> addPoints(pair <cpp_int, cpp_int> p, pair <cpp_int, cpp_int> q, cpp_int field) {
	while (p.first < 0)  p.first += field;
	while (p.second < 0) p.second += field;
	while (q.first < 0)  q.first += field;
	while (q.second < 0) q.second += field;

	cpp_int m, revEl;
	if (p.first == q.first) {
		if (p.second != q.second)
			return make_pair(-1, -1);
		revEl = advancedEuclid(2 * p.second % field, field).first;
		m = 3 * p.first * p.first * revEl % field;
	}
	else {
		revEl = advancedEuclid((p.first % field - q.first % field + field) % field, field).first;
		m = (p.second - q.second) * revEl % field;
	}
	cpp_int xR = (m * m - p.first - q.first) % field;
	cpp_int yR = -1 * (p.second + m * (xR - p.first)) % field;

	while (xR < 0) xR += field;
	while (yR < 0) yR += field;
	return make_pair(xR, yR);
}


pair <cpp_int, cpp_int> scalarMult(cpp_int n, pair <cpp_int, cpp_int> p, cpp_int field) {
	string nBin = binForm(n);
	reverse(nBin.begin(), nBin.end());

	pair <cpp_int, cpp_int> res = make_pair(0, 0);
	if (nBin[0] == '1')
		res = p;
	for (int i = 1; i < nBin.size(); i++) {
		p = addPoints(p, p, field);
		if (nBin[i] == '1' && res.first == 0 && res.second == 0)
			res = p;
		else if (nBin[i] == '1') {
			res = addPoints(res, p, field);
			if (res.first == -1 && res.second == -1 && i != nBin.size() - 1)
				return make_pair(-2, -2);
		}

	}
	return res;
}


void genElCurve(int l, int m) {
again:
	cpp_int p = genPrimeNum(l);
	cout << "\n\np = " << p;

	pair <cpp_int, cpp_int> c_d = decomposeP(3, p);
	if (c_d.first == 0 && c_d.second == 0)
		goto again;
	cout << "\nc = " << c_d.first << ", d = " << c_d.second << "\nc^2 + 3*d^2 = " << c_d.first * c_d.first + 3 * c_d.second * c_d.second;

	vector <cpp_int> T{ c_d.first + 3 * c_d.second, c_d.first - 3 * c_d.second, 2 * c_d.first,
						-(c_d.first + 3 * c_d.second), -(c_d.first - 3 * c_d.second), -(2 * c_d.first) };
	cpp_int N, r;
	for (int i = 0; i < T.size(); i++) {
		N = r = p + 1 + T[i];
		if (miller_rabin(r))                        break;
		else if (r % 3 == 0 && miller_rabin(r / 3)) r /= 3;
		else if (r % 2 == 0 && miller_rabin(r / 2)) r /= 2;
		else if (r % 6 == 0 && miller_rabin(r / 6)) r /= 6;
		else if (i == 5)                            goto again;
		else                                        continue;
		break;
	}
	cout << "\nN = " << N << ", r = " << r;

	if (p == r)
		goto again;
	for (int i = 1; i <= m; i++)
		if (powClosed(p, i, r) == 1)
			goto again;

genPoint:
	cpp_int x0 = rand() % p, y0 = rand() % p;
	while (x0 == 0)
		x0 = rand() % p;
	while (y0 == 0)
		y0 = rand() % p;
	cpp_int B = (y0 * y0 - x0 * x0 * x0) % p;
	if (N == r) {
		if (symbolLegendre(B, p) != -1 || cubeDecuct(B, p) != -1)
			goto genPoint;
	}
	else if (N == 3 * r) {
		if (symbolLegendre(B, p) != 1 || cubeDecuct(B, p) != -1)
			goto genPoint;
	}
	else if (N == 6 * r) {
		if (symbolLegendre(B, p) != 1 || cubeDecuct(B, p) != 1)
			goto genPoint;
	}
	else if (N == 2 * r) {
		if (symbolLegendre(B, p) != -1 || cubeDecuct(B, p) != 1)
			goto genPoint;
	}
	cout << "\nx0 = " << x0 << ", y0 = " << y0;

	pair <cpp_int, cpp_int> INF = scalarMult(N, make_pair(x0, y0), p);
	if (INF.first != -1 || INF.first == -2)
		goto genPoint;

	pair <cpp_int, cpp_int> Q = scalarMult(N / r, make_pair(x0, y0), p);
	cout << "\nQ = (" << Q.first << ", " << Q.second << ")";

	cout << "\nРезультат: p = " << p << ", B = " << B << ", Q = (" << Q.first << ", " << Q.second << "), r = " << r;

	ofstream fout;
	fout.open("points.txt");
	fout << Q.first << " " << Q.second;
	pair <cpp_int, cpp_int> point = Q;
	for (int i = 2; i < r; i++) {
		point = addPoints(point, Q, p);
		fout << endl << point.first << " " << point.second;
	}
	fout.close();
	cout << "\nЦиклическая подгруппа записана в файл points.txt";
}


int main() {
	srand(time(0));
	setlocale(LC_ALL, "ru");
	cout << "\tГенерация эллиптической кривой с j = 0";
	int l, m;
	cout << "\nДлина характеристики поля l (l > @): ";
	cin >> l;
	if (!l || l < 3) {
		cout << "Некорректный ввод данных! \n";
		return 0;
	}
	cout << "Параметр безопасности m: ";
	cin >> m;
	if (!m || m < 0) {
		cout << "Некорректный ввод данных! \n";
		return 0;
	}

	genElCurve(l, m);
	char build;
	cout << "\n\nПостроить график точек y/n? ";
	cin >> build;
	if (build == 'y')
		system("python ShowPoints.py");

	cout << endl;
	return 0;
}