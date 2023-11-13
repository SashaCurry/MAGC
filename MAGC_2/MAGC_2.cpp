changeParameters(pair <cpp_int, cpp_int>& P, cpp_int& s, set <pair <cpp_int, cpp_int>> tablePs, pair <cpp_int, cpp_int>& Q,
	pair <cpp_int, cpp_int>& R, vector <pair <cpp_int, cpp_int>> pairs_ij, vector <cpp_int>& ms, int a, int b, cpp_int p) {
	cout << endl;
	for (;;) {
		cout << "\n1 - продолжить \n2 - изменить P \n3 - изменить s \n4 - изменить таблицу из 2s + 1 точек \n5 - изменить Q";
		cout << "\n6 - изменить R \n7 - изменить пары (i, j) \n8 - изменить кандидатов на порядок ЭК \n";
		int choice;
		cin >> choice;
		switch (choice) {
		case 1:
			return true;
		case 2: {
			cout << "\nВведите новое значение P: ";
			cpp_int x, y;
			cin >> x >> y;
			if (!x || !y) {
				cout << "\nНеверные параметры! \n";
				return false;
			}
			cpp_int yy = (x * x * x + a * x + b) % p;
			if (y * y % p != yy) {
				cout << "\nТочка P = (" << x << ", " << y << ") не принадлежит эллиптической кривой! \n";
				return false;
			}
			P = make_pair(x, y);
		}
			  break;
		case 3: {
			cout << "\nВведите новое значение s: ";
			cpp_int sNew;
			cin >> sNew;
			if (sNew < 0 || !sNew) {
				cout << "\nНеверный параметр s! \n";
				return false;
			}
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
				if (!x || !y)
					return false;
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
	if (!changeParameters(P, s, tablePs, Q, R, pairs_ij, ms, a, b, p))
		return 0;

genS:
	s = sqrt(sqrt(p));
	if (pow(s, 4) != p)
		s += 1;
	cout << "\ns = " << s;
	if(!changeParameters(P, s, tablePs, Q, R, pairs_ij, ms, a, b, p))
		return 0;

	tablePs = { make_pair(-1, -1) };
	for (cpp_int i = 1; i <= s; i++) {
		pair <cpp_int, cpp_int> Pnew = scalarMult(i, P, a, p);
		tablePs.insert(Pnew);
		tablePs.insert(make_pair(Pnew.first, -Pnew.second + p));
	}
	cout << "\nТаблица из 2s + 1 точек: ";
	for (auto i : tablePs)
		cout << "(" << i.first << ", " << i.second << ") ";
	if(!changeParameters(P, s, tablePs, Q, R, pairs_ij, ms, a, b, p))
		return 0;

genQR:
	if (sqrt(sqrt(p)) != s && sqrt(sqrt(p)) != s - 1) {
		cout << "\nПараметры s является некорректным!";
		return 0;
	}
	Q = scalarMult(2 * s + 1, P, a, p);
	R = scalarMult(p + 1, P, a, p);
	cout << "\nQ = (" << Q.first << ", " << Q.second << ")";
	cout << "\nR = (" << R.first << ", " << R.second << ")";
	if (Q.first == -1) {
		cout << "\nТочка Q является некорректной!";
		return 0;
	}
	if(!changeParameters(P, s, tablePs, Q, R, pairs_ij, ms, a, b, p))
		return 0;
	if (Q.first == -1) {
		cout << "\nТочка Q является некорректной!";
		return 0;
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
	if(!changeParameters(P, s, tablePs, Q, R, pairs_ij, ms, a, b, p))
		return 0;

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
	if(!changeParameters(P, s, tablePs, Q, R, pairs_ij, ms, a, b, p))
		return 0;

	for (short k = 0; ms.size() > 1 && k < 1000; k++) {
		cpp_int x = rand() % (p - 2) + 2;
		cpp_int yy = (x * x * x + a * x + b) % p;
		if (symbolLegendre(yy, p) != 1)
			continue;
		cpp_int y = sqrtFromZp(yy, p);
		P = make_pair(x, y);
		for (int i = 0; i < ms.size(); i++) {
			//cout << "\nP = (" << P.first << ", " << P.second << ") * " << ms[i];
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
		//cout << "\nРезультат не является порядком ЭК! \n";
		cout << "\nВ ходе работы параметры были изменены! \n";
		return 0;
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
	if (m != 0)
		cout << "\nПорядок эллиптической кривой: " << m << endl;
	else
		return 0;
}
