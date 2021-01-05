// p.50,51に載っているものそのまま

comp(p, x) {
	int pc,v,i,k,m,S,n,a,b;

	if(executable(p,x) == 1) {
		k = p[1];
		m = p[2];
		S = p[3];
		n = length(S) + 1;

		v = sequence(0,m);
		i = 1;
		loop(k) {
			v[i] = x[i];
			i++;
		}

		pc = 1;

		while (pc<n){

			if (S[pc][1] == 1) {
				pc = S[pc][2];
			}
			else if (S[pc][1] == 2) {
				a = S[pc][2];
				b = S[pc][3];
				v[a] = b;
				pc++;
			}
			else if (S[pc][1] == 3) {
				a = S[pc][2];
				b = S[pc][3];
				v[a] = v[b];
				pc++;
			}
			else if (S[pc][1] == 4) {
				a = S[pc][2];
				v[a] = v[a] + 1;
				pc++;
			}
			else if (S[pc][1] == 5) {
				a = S[pc][2];
				if (v[a] != 0) v[a] = v[a] - 1;
				pc++;
			}
			else if (S[pc][1] == 6) {
				a = S[pc][2];
				b = S[pc][3];
				if (v[a] > 0) pc = b; else pc++;
			}
		}
		return (v[1]);


	} else {
		while (0 == 0) {}
	}
}


isCode(p) {
	int pc,k,m,S,l,max;

	if(length(p) < 3) return(0);

	k = p[1];
	m = p[2];
	S = p[3];
	l = length(S);
	max = 0;

	if(k < m) return(0);

	pc = 1;
	while(pc < l + 1) {
	 	if(S[pc][1] == 1) {
	 		if(length(S[pc]) != 2)
	 			return (0);

	 		a = S[pc][2];
	 		if(a>m) return (0);
	 		if(a>l) return (0);
	 		pc++;
	 	}
	 	else if(S[pc][1] == 2) {
	 		if(length(S[pc]) != 3)
	 			return (0);

	 		a = S[pc][2];
	 		b = S[pc][3];
	 		if(a>m) return (0);
	 		if(b>m) return (0);
	 		pc++;
	 	}
	 	else if(S[pc][1] == 3) {
	 		if(length(S[pc]) != 3)
	 			return 0;

	 		a = S[pc][2];
	 		b = S[pc][3];
	 		if(a>m) return (0);
	 		if(b>m) return (0);
	 		pc++;
	 	}
	 	else if(S[pc][1] == 4) {
	 		if(length(S[pc]) != 2)
	 			return(0);

	 		a = S[pc][2];
	 		if(a>m) return (0);
	 		pc++;
	 	}
	 	else if(S[pc][1] == 5) {
	 		if(length(S[pc]) != 2)
	 			return(0);

	 		a = S[pc][2];
	 		if(a>m) return (0);
	 		pc++;
	 	}
	 	else if(S[pc][1] == 6) {
	 		if(length(S[pc]) != 3)
	 			return(3);

			a = S[pc][2];
			b = S[pc][3];
			if(a>m) return (0);
			if(b>m) return (0);
			if(b>l) return (0);
	 		pc++;
	 	}
	 }
}


executable(p, x) {
	int ak, xk;

	if(isCode(p) == 0) return 0;

	ak = p[1];		 // pの引数の個数
	xk = length(x);	 // xをデコードしたものの長さ
	if(ak ==  xk) return 1;
	return 0;
}

element(a, i){
	int b, j;

	b = a;
	j = 1; 						// 要素は1始まり
	loop(length(a)) {
		if (j == i)
			return (left(b));
		else
			b = right(b);
		j++;
	}
}

sequence(x, k){
	int r;

	r = 0;
	loop(k) {
		r = pair(x, r)
	}
	return (r);
}

replace(a, i, x){
	int b, j;
	b = 0;
	j = length(a);
	loop(j) {
		if (j == i)
			b = pair(x, b);
		else
			b = pair(element(a, j), b);
		j--;
	}
	return(b);
}

length(a) {
	int l, b;

	if(a == 0) return(0);	// 0の場合は空列なので0

	l = 1;
	b = a;

	loop(a) {
		if(right(b) != 0) {
			b = right(b);
			l++;
		} else {
			return (l);
		}
	}
}

left(z) {
	int x, y;

	if(z == 0) {
		return (0);
	}

	x = 0;
	loop(z) {
		y = 0;
		loop(z) {
			if (pair(x, y) == z) {
				return (x);
			}
			y++;
		}
		x++;
	}
}

right(z) {
	int x, y;

	if(z == 0) {
		return (0);
	}

	x = 0;
	loop(z) {
		y = 0;
		loop(z) {
			if (pair(x, y) == z) {
				return (y);
			}
			y++;
		}
		x++;
	}
}

pair(x, y) {
	return(((x+y)* (x+y+1) / 2) + x + 1);
}