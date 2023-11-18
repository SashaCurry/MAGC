#include "iostream"
#include "vector"
#include "string"
#include "algorithm"
#include "fstream"
#include "map"
#include "set"
#include "boost/multiprecision/cpp_int.hpp"

using namespace std;
using namespace boost::multiprecision;

map <char, string> book{ {'0', "11"}, {'1', "12"}, {'2', "13"}, {'3', "14"}, {'4', "15"}, {'5', "16"}, {'6', "17"}, {'7', "18"}, {'8', "19"}, {'9', "21"},
						 {' ', "22"}, {'!', "23"}, {'"', "24"}, {'#', "25"}, {'$', "26"}, {'%', "27"}, {'^', "28"}, {'&', "29"}, {'\'', "31"}, {'(', "32"},
						 {')', "33"}, {'*', "34"}, {'+', "35"}, {',', "36"}, {'-', "37"}, {'.', "38"}, {'/', "39"}, {':', "41"}, {';', "42"}, {'<', "43"},
						 {'=', "44"}, {'>', "45"}, {'?', "46"}, {'@', "47"}, {'[', "48"}, {'\\', "49"}, {']', "51"}, {'_', "52"}, {'`', "53"}, {'{', "54"},
						 {'}', "55"}, {'|', "56"}, {'~', "57"}, {'\n', "58"}, {'а', "59"}, {'б', "61"}, {'в', "62"}, {'г', "63"}, {'д', "64"}, {'е', "65"},
						 {'ё', "66"}, {'ж', "67"}, {'з', "68"}, {'и', "69"}, {'й', "71"}, {'к', "72"}, {'л', "73"}, {'м', "74"}, {'н', "75"}, {'о', "76"},
						 {'п', "77"}, {'р', "78"}, {'с', "79"}, {'т', "81"}, {'у', "82"}, {'ф', "83"}, {'х', "84"}, {'ц', "85"}, {'ч', "86"}, {'ш', "87"},
						 {'щ', "88"}, {'ъ', "89"}, {'ы', "91"}, {'ь', "92"}, {'э', "93"}, {'ю', "94"}, {'я', "95"} };

map <string, char> bookRvs{ {"11", '0'}, {"12", '1'}, {"13", '2'}, {"14", '3'}, {"15", '4'}, {"16", '5'}, {"17", '6'}, {"18", '7'}, {"19", '8'}, {"21", '9'},
							{"22", ' '}, {"23", '!'}, {"24", '"'}, {"25", '#'}, {"26", '$'}, {"27", '%'}, {"28", '^'}, {"29", '&'}, {"31", '\''}, {"32", '('},
							{"33", ')'}, {"34", '*'}, {"35", '+'}, {"36", ','}, {"37", '-'}, {"38", '.'}, {"39", '/'}, {"41", ':'}, {"42", ';'}, {"43", '<'},
							{"44", '='}, {"45", '>'}, {"46", '?'}, {"47", '@'}, {"48", '['}, {"49", '\\'}, {"51", ']'}, {"52", '_'}, {"53", '`'}, {"54", '{'},
							{"55", '}'}, {"56", '|'}, {"57", '~'}, {"58", '\n'}, {"59", 'а'}, {"61", 'б'}, {"62", 'в'}, {"63", 'г'}, {"64", 'д'}, {"65", 'е'},
							{"66", 'ё'}, {"67", 'ж'}, {"68", 'з'}, {"69", 'и'}, {"71", 'й'}, {"72", 'к'}, {"73", 'л'}, {"74", 'м'}, {"75", 'н'}, {"76", 'о'},
							{"77", 'п'}, {"78", 'р'}, {"79", 'с'}, {"81", 'т'}, {"82", 'у'}, {"83", 'ф'}, {"84", 'х'}, {"85", 'ц'}, {"86", 'ч'}, {"87", 'ш'},
							{"88", 'щ'}, {"89", 'ъ'}, {"91", 'ы'}, {"92", 'ь'}, {"93", 'э'}, {"94", 'ю'}, {"95", 'я'} };

set <char> upSymbs{ 'А', 'Б', 'В', 'Г', 'Д', 'Е', 'Ё', 'Ж', 'З', 'И', 'Й', 'К', 'Л', 'М', 'Н', 'О', 'П',
					'Р', 'С', 'Т', 'У', 'Ф', 'Ч', 'Ц', 'Ч', 'Ш', 'Щ', 'Ъ', 'Ы', 'Ь', 'Э', 'Ю', 'Я' };

class Pattern {
private:
	static vector <cpp_int> deg2(cpp_int el, cpp_int n) {
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


	static cpp_int multMod(cpp_int n, cpp_int mod, vector <pair <cpp_int, cpp_int>> lst) {
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
public:
	static cpp_int powClosed(cpp_int x, cpp_int y, cpp_int mod) {
		if (y == 0)
			return 1;

		vector <cpp_int> lst = deg2(1, y);
		vector <pair <cpp_int, cpp_int>> xDegs;
		for (int i = 0; i < lst.size(); i++)
			xDegs.push_back(make_pair(x, lst[i]));

		cpp_int res = multMod(x, mod, xDegs);
		return res;
	}


	static string binForm(cpp_int x) {
		string bitter = "";
		while (x != 0) {
			bitter = (x % 2 == 0 ? "0" : "1") + bitter;
			x = x / 2;
		}
		if (bitter == "")
			return "0";
		return bitter;
	}


	static	cpp_int decForm(string x) {
		cpp_int res = 0, deg = 1;
		if (!x.empty() && x.back() == '1')
			res += 1;
		for (short i = x.length() - 2; i >= 0; i--) {
			deg = deg * 2;
			if (x[i] == '1')
				res += deg;
		}
		return res;
	}


	static pair <cpp_int, cpp_int> advancedEuclid(cpp_int a, cpp_int b) {
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


	static bool checkStrDigit(string str) {
		if (str.empty())
			return false;
		for (int i = 0; i < str.length(); i++)
			if (!isdigit(str[i]))
				return false;
		return true;
	}


	static bool miller_rabin(cpp_int n, int k = 10) {
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

			cpp_int x = Pattern::powClosed(a, d, n);
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

	static cpp_int hashFun(cpp_int m, pair <cpp_int, cpp_int> R) {
		hash <cpp_int> hashCpp_int;
		string res = to_string(hashCpp_int(m)) + to_string(hashCpp_int(R.first)) + to_string(hashCpp_int(R.second));
		return cpp_int(res);
	}
};



class PointArithmetic {
public:
	static pair <cpp_int, cpp_int> addPoints(pair <cpp_int, cpp_int> p, pair <cpp_int, cpp_int> q, cpp_int field) {
		while (p.first < 0)  p.first += field;
		while (p.second < 0) p.second += field;
		while (q.first < 0)  q.first += field;
		while (q.second < 0) q.second += field;

		cpp_int m, revEl;
		if (p.first == q.first) {
			if (p.second != q.second)
				return make_pair(-1, -1);
			revEl = Pattern::advancedEuclid(2 * p.second % field, field).first;
			m = 3 * p.first * p.first * revEl % field;
		}
		else {
			revEl = Pattern::advancedEuclid((p.first % field - q.first % field + field) % field, field).first;
			m = (p.second - q.second) * revEl % field;
		}
		cpp_int xR = (m * m - p.first - q.first) % field;
		cpp_int yR = -1 * (p.second + m * (xR - p.first)) % field;

		while (xR < 0) xR += field;
		while (yR < 0) yR += field;
		return make_pair(xR, yR);
	}


	static pair <cpp_int, cpp_int> scalarMult(cpp_int n, pair <cpp_int, cpp_int> p, cpp_int field) {
		string nBin = Pattern::binForm(n);
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
};



class EllipticCurve {
private:
	cpp_int p;
	cpp_int B;
	pair <cpp_int, cpp_int> Q;
	cpp_int r;


	cpp_int pow(cpp_int x, cpp_int y) {
		cpp_int res = 1;
		for (int i = 0; i < y; i++)
			res *= x;
		return res;
	}


	cpp_int genPrimeNum(int l) {
		string pBin = "1", pBinMax = "1", pBinMin = "1";
		for (int i = 1; i <= l - 2; i++) {
			pBin += to_string(rand() % 2);
			pBinMax += "1";
			pBinMin += "0";
		}
		pBin += "1"; pBinMax += "1"; pBinMin += "1";

		cpp_int p = Pattern::decForm(pBin), pMax = Pattern::decForm(pBinMax), pMin = Pattern::decForm(pBinMin);
		while (p % 6 != 1)
			p--;
		if (p < pMin)
			p += 6;
		while (pMin % 6 != 1)
			pMin++;

		for (p; p <= pMax; p += 6)
			if (Pattern::miller_rabin(p))
				return p;
		for (pMin; pMin < p; pMin += 6)
			if (Pattern::miller_rabin(pMin))
				return pMin;
	}


	cpp_int symbolLegendre(cpp_int a, cpp_int p) {
		if (a % p == 0)
			return 0;
		while (a < 0)
			a += p;
		cpp_int res = Pattern::powClosed(a, (p - 1) / 2, p);
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
			while (Pattern::powClosed(a, pow(2, k) * q, p) != 1)
				k++;
			kArr.push_back(k);
			if (k == 0)
				break;
			a = (a * pow(b, pow(2, m - kArr.back()))) % p;
		}

		cpp_int r = Pattern::powClosed(a, (q + 1) / 2, p);
		for (int i = kArr.size() - 2; i >= 0; i--)
			r = (r * Pattern::advancedEuclid(pow(b, pow(2, m - kArr[i] - 1)), p).first) % p;

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
		cpp_int res = Pattern::powClosed(a, (p - 1) / 3, p);
		return res == 1 ? 1 : -1;
	}


	void genElCurve(int l, int m) {
	again:
		cpp_int p = genPrimeNum(l);
		this->p = p;

		pair <cpp_int, cpp_int> c_d = decomposeP(3, p);
		if (c_d.first == 0 && c_d.second == 0)
			goto again;

		vector <cpp_int> T{ c_d.first + 3 * c_d.second, c_d.first - 3 * c_d.second, 2 * c_d.first,
							-(c_d.first + 3 * c_d.second), -(c_d.first - 3 * c_d.second), -(2 * c_d.first) };
		cpp_int N, r;
		for (int i = 0; i < T.size(); i++) {
			N = r = p + 1 + T[i];
			if (Pattern::miller_rabin(r))                        break;
			else if (r % 3 == 0 && Pattern::miller_rabin(r / 3)) r /= 3;
			else if (r % 2 == 0 && Pattern::miller_rabin(r / 2)) r /= 2;
			else if (r % 6 == 0 && Pattern::miller_rabin(r / 6)) r /= 6;
			else if (i == 5)                                     goto again;
			else                                                 continue;
			break;
		}
		this->r = r;

		if (p == r)
			goto again;
		for (int l = 1; l <= m; l++)
			if (Pattern::powClosed(p, l, r) == 1)
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
		this->B = B < 0 ? B + p : B;

		pair <cpp_int, cpp_int> INF = PointArithmetic::scalarMult(N, make_pair(x0, y0), p);
		if (INF.first != -1 || INF.first == -2)
			goto genPoint;

		pair <cpp_int, cpp_int> Q = PointArithmetic::scalarMult(N / r, make_pair(x0, y0), p);
		if (Q.first == -1)
			goto genPoint;
		this->Q = Q;
	}
public:
	EllipticCurve(int l, int m) {
		genElCurve(l, m);
	}
	~EllipticCurve() {
	}

	cpp_int get_p() {
		return this->p;
	}

	cpp_int get_b() {
		return this->B;
	}

	pair <cpp_int, cpp_int> get_Q() {
		return this->Q;
	}

	cpp_int get_r() {
		return this->r;
	}
};



class File {
public:
	static string read(string inFile) {
		ifstream fin(inFile);
		if (!fin.is_open())
			throw string{ "Файл " + inFile + " не найден! \n" };

		string str = "";
		for (char el; fin.get(el);)
			str += el;

		fin.close();
		return str;
	}

	static void write(string str, string outFile) {
		ofstream fout(outFile);
		if (!fout.is_open())
			throw string{ "Недостаточно прав для создания файла " + outFile + "!\n" };

		fout << str;
		fout.close();
	}

	static void append(string str, string outFile) {
		ofstream fout(outFile, ios::app);
		if (!fout.is_open())
			throw string{ "Файл " + outFile + " не найден! \n" };

		fout << endl << str;
		fout.close();
	}
};



void genOpenKeys() {
	int startL, startM;
	cout << "\nДлина характеристики поля l (l > 2): ";
	cin >> startL;
	while (!startL || startL < 3) {
		cout << "Некорректный ввод данных! \n";
		cin >> startL;
	}
	cout << "Параметр безопасности m: ";
	cin >> startM;
	while (!startM || startM < 0) {
		cout << "Некорректный ввод данных! \n";
		cin >> startM;
	}

	EllipticCurve E(startL, startM);

	File::write(to_string(E.get_p()), "p,b,Q,r.txt");
	File::append(to_string(E.get_b()), "p,b,Q,r.txt");
	string generator = to_string(E.get_Q().first) + " " + to_string(E.get_Q().second);
	File::append(generator, "p,b,Q,r.txt");
	File::append(to_string(E.get_r()) + '\n', "p,b,Q,r.txt");

	short lBinSize = rand() % 30 + 5;
	string lBin = "";
	for (short i = 0; i < lBinSize; i++)
		lBin += to_string(rand() % 2);
	cpp_int l = Pattern::decForm(lBin) % (E.get_r() - 1) + 1;
	File::write(to_string(l), "l.txt");

	pair <cpp_int, cpp_int> P = PointArithmetic::scalarMult(l, E.get_Q(), E.get_p());
	string PinFile = to_string(P.first) + " " + to_string(P.second);
	File::write(PinFile, "P.txt");

	cout << "\np = " << E.get_p() << "\nb = " << E.get_b() << "\nQ = (" << E.get_Q().first << ", " << E.get_Q().second << ")";
	cout << "\nr = " << E.get_r() << "\nl = " << l << "\nP = (" << P.first << ", " << P.second << ")\nКлючи записаны в файлы \n";
}


void createSignature() {
	string str;
	str = File::read("l.txt");
	if (!Pattern::checkStrDigit(str)) {
		throw string{ "Файл l.txt был изменён!" };
	}
	cpp_int l = cpp_int(str);

	pair <cpp_int, cpp_int> P;
	str = File::read("P.txt");
	for (short i = 0; i < str.length(); i++)
		if (str[i] == ' ') {
			if (Pattern::checkStrDigit(str.substr(0, i)) && Pattern::checkStrDigit(str.substr(i + 1)))
				P = make_pair(cpp_int(str.substr(0, i)), cpp_int(str.substr(i + 1)));
			else
				throw string{ "Файл P.txt был изменён!" };
		}

	cpp_int p, b, r;
	pair <cpp_int, cpp_int> Q;
	str = File::read("p,b,Q,r.txt");
	short startNum = 0;
	char part = 'p';
	for (short i = 0; i <= str.length(); i++)
		if (str[i] == '\n' || str[i] == ' ') {
			if (Pattern::checkStrDigit(str.substr(startNum, i - startNum))) {
				switch (part) {
				case 'p':
					p = cpp_int(str.substr(0, i - startNum));
					part = 'b';
					startNum = i + 1;
					break;
				case 'b':
					b = cpp_int(str.substr(startNum, i - startNum));
					part = 'Q';
					startNum = i + 1;
					break;
				case 'Q':
					if (str[i] == ' ') {
						Q.first = cpp_int(str.substr(startNum, i - startNum));
						startNum = i + 1;
					}
					else {
						Q.second = cpp_int(str.substr(startNum, i - startNum));
						part = 'r';
						startNum = i + 1;
					}
					break;
				case 'r':
					r = cpp_int(str.substr(startNum, i - startNum));
					break;
				}
			}
			else
				throw string{ "Файл p,b,Q,r.txt был изменён!" };
		}

	if (!Pattern::miller_rabin(p))
		throw string{ "\nПараметр p = " + to_string(p) + " не является простым числом!" };
	if (b < 1 || b > p - 1)
		throw string{ "\nПараметр b = " + to_string(b) + " должен быть в диапазоне 1 < b < p" };
	if (Q.second * Q.second % p != (Q.first * Q.first * Q.first + b) % p)
		throw string{ "\nПараметр Q = (" + to_string(Q.first) + ", " + to_string(Q.second) + ") не принадлежит кривой!" };
	if (PointArithmetic::scalarMult(r, Q, p).first != -1)
		throw string{ "\nПараметр Q = (" + to_string(Q.first) + ", " + to_string(Q.second) + ") или r = " + to_string(r) + " не является образующим!" };
	if (PointArithmetic::scalarMult(l, Q, p) != P || P.second * P.second % p != (P.first * P.first * P.first + b) % p)
		throw string{ "\nПараметр P = (" + to_string(P.first) + ", " + to_string(P.second) + ") не принадлежит кривой или P не равен lQ!" };
	if (l < 1 || l > r - 1)
		throw string{ "\nПараметр l = " + to_string(l) + " должен быть в диапазоне 1 < l < r" };

	str = File::read("m.txt");
	string mCode = "";
	for (short i = 0; i < str.length(); i++) {
		if (upSymbs.find(str[i]) != upSymbs.end())
			mCode += book[char(str[i] + 32)];
		else
			mCode += book[str[i]];
	}
	cpp_int m = cpp_int(mCode);

genK:
	cpp_int k = rand() % (r - 1) + 1;
	pair <cpp_int, cpp_int> R = PointArithmetic::scalarMult(k, Q, p);

	cpp_int e = Pattern::hashFun(m, R) % r;
	if (e == 0)
		goto genK;
	cpp_int s = (l * e + k) % r;

	File::write(to_string(e), "e.txt");
	File::write(to_string(s), "s.txt");
	cout << "\nm = " << m << "\ne = " << e << "\ns = " << s << "\nДанные успешно записаны в файлы \n";
}


void checkSignature() {
	string str;

	pair <cpp_int, cpp_int> P;
	str = File::read("P.txt");
	for (short i = 0; i < str.length(); i++)
		if (str[i] == ' ') {
			if (Pattern::checkStrDigit(str.substr(0, i)) && Pattern::checkStrDigit(str.substr(i + 1)))
				P = make_pair(cpp_int(str.substr(0, i)), cpp_int(str.substr(i + 1)));
			else
				throw string{ "Файл P.txt был изменён!" };
		}

	cpp_int p, b, r;
	pair <cpp_int, cpp_int> Q;
	str = File::read("p,b,Q,r.txt");
	short startNum = 0;
	char part = 'p';
	for (short i = 0; i <= str.length(); i++)
		if (str[i] == '\n' || str[i] == ' ') {
			if (Pattern::checkStrDigit(str.substr(startNum, i - startNum))) {
				switch (part) {
				case 'p':
					p = cpp_int(str.substr(0, i - startNum));
					part = 'b';
					startNum = i + 1;
					break;
				case 'b':
					b = cpp_int(str.substr(startNum, i - startNum));
					part = 'Q';
					startNum = i + 1;
					break;
				case 'Q':
					if (str[i] == ' ') {
						Q.first = cpp_int(str.substr(startNum, i - startNum));
						startNum = i + 1;
					}
					else {
						Q.second = cpp_int(str.substr(startNum, i - startNum));
						part = 'r';
						startNum = i + 1;
					}
					break;
				case 'r':
					r = cpp_int(str.substr(startNum, i - startNum));
					break;
				}
			}
			else
				throw string{ "Файл p,b,Q,r.txt был изменён!" };
		}

	str = File::read("m.txt");
	string mCode = "";
	for (short i = 0; i < str.length(); i++) {
		if (upSymbs.find(str[i]) != upSymbs.end())
			mCode += book[char(str[i] + 32)];
		else
			mCode += book[str[i]];
	}
	cpp_int m = cpp_int(mCode);

	cpp_int s = cpp_int(File::read("s.txt"));
	cpp_int e = cpp_int(File::read("e.txt"));

	if (!Pattern::miller_rabin(p))
		throw string{ "\nПараметр p = " + to_string(p) + " не является простым числом!" };
	if (b < 1 || b > p - 1)
		throw string{ "\nПараметр b = " + to_string(b) + " должен быть в диапазоне 1 < b < p" };
	if (Q.second * Q.second % p != (Q.first * Q.first * Q.first + b) % p)
		throw string{ "\nПараметр Q = (" + to_string(Q.first) + ", " + to_string(Q.second) + ") не принадлежит кривой!" };
	if (PointArithmetic::scalarMult(r, Q, p).first != -1)
		throw string{ "\nПараметр Q = (" + to_string(Q.first) + ", " + to_string(Q.second) + ") или r = " + to_string(r) + " не является образующим!" };
	if (P.second * P.second % p != (P.first * P.first * P.first + b) % p)
		throw string{ "\nПараметр P = (" + to_string(P.first) + ", " + to_string(P.second) + ") не принадлежит кривой" };

	pair <cpp_int, cpp_int> sQ = PointArithmetic::scalarMult(s, Q, p);
	pair <cpp_int, cpp_int> eP = PointArithmetic::scalarMult(e, P, p);
	pair <cpp_int, cpp_int> R_ = PointArithmetic::addPoints(sQ, make_pair(eP.first, -eP.second), p);
	cpp_int e_ = Pattern::hashFun(m, R_) % r;

	cout << "\nR\' = (" << R_.first << ", " << R_.second << ")" << "\ne\' = " << e_ << "\ne = " << e;
	if (e_ == e)
		cout << "\nПодпись действительна \n";
	else
		cout << "\nПодпись недействительна \n";
}


int main() {
	srand(time(0));
	setlocale(LC_ALL, "ru");
	cout << "\tПротокол цифровой подписи Шнорра на эллиптических кривых";

	string choice;
	for (;;) {
		cout << "\n1 - Генерация ключей \n2 - Создание подписи \n3 - Проверка подписи \n";
		cin >> choice;
		try {
			if (choice == "1")
				genOpenKeys();
			else if (choice == "2")
				createSignature();
			else if (choice == "3")
				checkSignature();
			else
				cout << "\nIncorrect! Try again \n";
		}
		catch (string& message) {
			cout << endl << message << endl;
		}
	}
	return 0;
}