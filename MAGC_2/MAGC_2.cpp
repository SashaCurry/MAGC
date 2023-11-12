#include "iostream"
#include "vector"
#include "string"
#include "set"
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


pair <cpp_int, cpp_int> addPoints(pair <cpp_int, cpp_int> p, pair <cpp_int, cpp_int> q, int a, cpp_int field) {
	if (p.first == -1 && p.second == -1)
		return q;
	if (q.first == -1 && q.second == -1)
		return p;

	while (p.first < 0)  p.first += field;
	while (p.second < 0) p.second += field;
	while (q.first < 0)  q.first += field;
	while (q.second < 0) q.second += field;
	p.first %= field, p.second %= field;
	q.first %= field, q.second %= field;

	cpp_int m, revEl;
	if (p.first == q.first) {
		if (p.second != q.second)
			return make_pair(-1, -1);
		revEl = advancedEuclid(2 * p.second % field, field).first;
		m = (3 * p.first * p.first + a) * revEl % field;
	}
	else {
		revEl = advancedEuclid((p.first - q.first + field) % field, field).first;
		m = (p.second - q.second) * revEl % field;
	}
	cpp_int xR = (m * m - p.first - q.first) % field;
	cpp_int yR = -1 * (p.second + m * (xR - p.first)) % field;

	while (xR < 0) xR += field;
	while (yR < 0) yR += field;
	return make_pair(xR, yR);
}


pair <cpp_int, cpp_int> scalarMult(cpp_int n, pair <cpp_int, cpp_int> p, int a, cpp_int field) {
	string nBin = binForm(n);
	reverse(nBin.begin(), nBin.end());

	pair <cpp_int, cpp_int> res = make_pair(-1, -1);
	if (nBin[0] == '1')
		res = p;
	for (int i = 1; i < nBin.size(); i++) {
		p = addPoints(p, p, a, field);
		if (nBin[i] == '1' && res.first == 0 && res.second == 0)
			res = p;
		else if (nBin[i] == '1') {
			res = addPoints(res, p, a, field);
			if (res.first == -1 && res.second == -1 && i != nBin.size() - 1)
				return make_pair(-2, -2);
		}

	}
	return res;
}


void changeParameters(pair <cpp_int, cpp_int>& P, cpp_int& s, set <pair <cpp_int, cpp_int>> tablePs, pair <cpp_int, cpp_int>& Q,
	pair <cpp_int, cpp_int>& R, vector <pair <cpp_int, cpp_int>> pairs_ij, vector <cpp_int>& ms, int a, int b, cpp_int p) {
	cout << endl;
	for (;;) {
		cout << "\n1 - продолжить \n2 - изменить P \n3 - изменить s \n4 - изменить таблицу из 2s + 1 точек \n5 - изменить Q";
		cout << "\n6 - изменить R \n7 - изменить пары (i, j) \n8 - изменить кандидатов на порядок ЭК \n";
		int choice;
		cin >> choice;
		switch (choice) {
		case 1:
			return;
		case 2: {
			cout << "\nВведите новое значение P: ";
			cpp_int x, y;
			cin >> x >> y;
			cpp_int yy = (x * x * x + a * x + b) % p;
			if (y * y % p != yy)
				cout << "\nТочка P = (" << x << ", " << y << ") не принадлежит эллиптической кривой! \nP = (" << P.first << ", " << P.second << ") \n";
			else
				P = make_pair(x, y);
		}
			  break;
		case 3: {
			cout << "\nВведите новое значение s: ";
			cpp_int sNew;
			cin >> sNew;
			if (sNew < 0)
				cout << "\nПараметра s не может быть меньше 0! \ns = " << s << endl;
			else
				s = sNew;
		}
			  break;
		case 4: {
			tablePs.clear();
			cout << "\nВведите количество точек в таблице: ";
			short n;
			cin >> n;
			cout << "Введите новые точки: ";
			cpp_int x, y;
			for (short i = 0; i < n; i++) {
				cin >> x >> y;
				tablePs.insert(make_pair(x, y));
			}
		}
			  break;
		case 5: {
			cout << "\nВведите новое значение Q: ";
			cin >> Q.first >> Q.second;
			if (Q.first < 0 && !(Q.first == -1 && Q.second == -1))
				Q.first += p;
			if (Q.second < 0 && !(Q.first == -1 && Q.second == -1))
				Q.second += p;
		}
			  break;
		case 6:
			cout << "\nВведите новое значение R: ";
			cin >> R.first >> R.second;
			if (R.first < 0 && !(R.first == -1 && R.second == -1))
				R.first += p;
			if (R.second < 0 && !(R.first == -1 && R.second == -1))
				R.second += p;
			break;
		case 7: {
			pairs_ij.clear();
			cout << "\nВведите количество новых пар: ";
			short n;
			cin >> n;
			cout << "Введите новые пары (i, j): ";
			cpp_int i, j;
			for (short k = 0; k < n; k++) {
				cin >> i >> j;
				pairs_ij.push_back(make_pair(i, j));
			}
		}
			  break;
		case 8: {
			ms.clear();
			cout << "Введите количество новых кандидатов на порядок ЭК: ";
			short n;
			cin >> n;
			cout << "Введите новых кандидатов: ";
			cpp_int m;
			for (short i = 0; i < n; i++) {
				cin >> m;
				ms.push_back(m);
			}
		}
			  break;
		default:
			cout << "Incorrect! Try again";
		}
	}
}


void putUniqueM(vector <cpp_int>& ms, cpp_int m) {

	for (int i = 0; i < ms.size(); i++) {
		if (max(ms[i], m) % min(ms[i], m) == 0) {
			if (ms[i] > m)
				ms.erase(ms.begin() + i);
			else {
				m = ms[i];
				ms.erase(ms.begin() + i);
			}
			i--;
		}
	}
	ms.push_back(m);
}


cpp_int giantStep_babyStep(int a, int b, cpp_int p) {
	pair <cpp_int, cpp_int> P = make_pair(0, 0);
	cpp_int s = 0;
	set <pair <cpp_int, cpp_int>> tablePs;
	pair <cpp_int, cpp_int> Q = make_pair(0, 0);
	pair <cpp_int, cpp_int> R = make_pair(0, 0);
	vector <pair <cpp_int, cpp_int>> pairs_ij;
	vector <cpp_int> ms;

genPoint:
	cpp_int x = rand() % (p - 2) + 2;
	cpp_int yy = (x * x * x + a * x + b) % p;
	if (symbolLegendre(yy, p) != 1)
		goto genPoint;
	cpp_int y = sqrtFromZp(yy, p);
	if (y < 0)
		y += p;
	P = make_pair(x, y);
	cout << "\nP = (" << P.first << ", " << P.second << ")";
	changeParameters(P, s, tablePs, Q, R, pairs_ij, ms, a, b, p);

genS:
	s = sqrt(sqrt(p));
	if (pow(s, 4) != p)
		s += 1;
	cout << "\ns = " << s;
	changeParameters(P, s, tablePs, Q, R, pairs_ij, ms, a, b, p);

	tablePs = { make_pair(-1, -1) };
	for (cpp_int i = 1; i <= s; i++) {
		pair <cpp_int, cpp_int> Pnew = scalarMult(i, P, a, p);
		tablePs.insert(Pnew);
		tablePs.insert(make_pair(Pnew.first, -Pnew.second + p));
	}
	cout << "\nТаблица из 2s + 1 точек: ";
	for (auto i : tablePs)
		cout << "(" << i.first << ", " << i.second << ") ";
	changeParameters(P, s, tablePs, Q, R, pairs_ij, ms, a, b, p);

genQR:
	if (sqrt(sqrt(p)) != s && sqrt(sqrt(p)) != s - 1) {
		cout << "\nПараметры s является некорректным! Вычисление параметра заново";
		goto genS;
	}
	Q = scalarMult(2 * s + 1, P, a, p);
	R = scalarMult(p + 1, P, a, p);
	cout << "\nQ = (" << Q.first << ", " << Q.second << ")";
	cout << "\nR = (" << R.first << ", " << R.second << ")";
	if (Q.first == -1) {
		cout << "\nТочка Q является некорректной! Вычисление точки P заново";
		goto genPoint;
	}
	changeParameters(P, s, tablePs, Q, R, pairs_ij, ms, a, b, p);

	if (Q.first == -1) {
		cout << "\nТочка Q является некорректной! Вычисление точки Q заново";
		goto genQR;
	}
	for (cpp_int i = 0; i <= s; i++) {
		pair <cpp_int, cpp_int> iQ = scalarMult(i, Q, a, p);
		pair <cpp_int, cpp_int> posRiQ = addPoints(R, iQ, a, p);
		if (tablePs.find(posRiQ) != tablePs.end())
			for (cpp_int j = -s; j <= s; j++)
				pairs_ij.push_back(make_pair(i, j));

		pair <cpp_int, cpp_int> negRiQ = addPoints(R, make_pair(iQ.first, -iQ.second + p), a, p);
		if (tablePs.find(negRiQ) != tablePs.end())
			for (cpp_int j = -s; j <= s; j++)
				pairs_ij.push_back(make_pair(-i, j));
	}
	cout << "\nПары (i, j): ";
	for (int i = 0; i < pairs_ij.size(); i++)
		cout << "(" << pairs_ij[i].first << ", " << pairs_ij[i].second << ") ";
	changeParameters(P, s, tablePs, Q, R, pairs_ij, ms, a, b, p);

	cpp_int infinum = p + 1 - 2 * sqrt(p), supremum = p + 1 + 2 * sqrt(p);
	for (int i = 0; i < pairs_ij.size(); i++) {
		cpp_int m = p + 1 + (2 * s + 1) * pairs_ij[i].first - pairs_ij[i].second;
		cout << "\nПри i = " << pairs_ij[i].first << " и j = " << pairs_ij[i].second << ", m = " << m;
		if (scalarMult(m, P, a, p).first == -1 && m > 0 && infinum < m && m < supremum)
			putUniqueM(ms, m);
	}
	cout << "\nКандидаты на порядок ЭК: ";
	for (int i = 0; i < ms.size(); i++)
		cout << ms[i] << " ";
	changeParameters(P, s, tablePs, Q, R, pairs_ij, ms, a, b, p);

	for (short k = 0; ms.size() > 1 && k < 1000; k++) {
		cpp_int x = rand() % (p - 2) + 2;
		cpp_int yy = (x * x * x + a * x + b) % p;
		if (symbolLegendre(yy, p) != 1)
			continue;
		cpp_int y = sqrtFromZp(yy, p);
		P = make_pair(x, y);
		for (int i = 0; i < ms.size(); i++) {
			cout << "\nP = (" << P.first << ", " << P.second << ") * " << ms[i];
			if (scalarMult(ms[i], P, a, p).first != -1) {
				ms.erase(ms.begin() + i);
				i--;
			}
		}
	}

	cpp_int mRes = ms[0];
	for (short i = 1; i < ms.size(); i++)
		if (ms[i] < mRes)
			mRes = ms[i];
	if (scalarMult(mRes, P, a, p).first != -1) {
		cout << "\nРезультат не является порядком ЭК! Выполнение алгоритма заново \n";
		goto genPoint;
	}
	return mRes;
}


int main() {
	srand(time(0));
	setlocale(LC_ALL, "ru");
	cout << "\tПоиск числа точек эллиптической кривой y^2 = x^3 + ax + b c помощью алгоритма \"giant step - baby step\"";
	int a, b;
	cpp_int p;
	cout << "\nВведите параметры эллиптической кривой a, b, p: ";
	cin >> a >> b >> p;

	if (!a || !b || !p) {
		cout << "\nНеверный параметр a, b или p! \n";
		return 0;
	}
	else if ((4 * a * a * a + 27 * b * b) % p == 0) {
		cout << "\nДанная эллиптическая кривая вырожденная! \n";
		return 0;
	}
	while (!miller_rabin(p)) {
		cout << "\nр - не простое число! \nВведите р: ";
		cin >> p;
	}

	cpp_int m = giantStep_babyStep(a, b, p);
	cout << "\nПорядок эллиптической кривой: " << m << endl;
	return 0;
}
