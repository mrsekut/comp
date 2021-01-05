// comp1に最小の手間を加え、C処理系で動くようにする
// loopをforに変えるなど

#include <gmpxx.h>
#include <iostream>
#include <time.h>


using namespace std;


/**
 * pair
 * =================================================================
 */
mpz_class pair(mpz_class x, mpz_class y) {
	return(((x+y) * (x+y+1) / 2) + x + 1);
}

mpz_class left(mpz_class z) {
	mpz_class x, y, i;

	if(z == 0) {
		return (0);
	}

	x = 0;
	for(i=0; i<z; i++) {
		y = 0;
		for(i=0; i<z; i++) {
			if (::pair(x, y) == z) {
				return (x);
			}
			y++;
		}
		x++;
	}
	return (0);
}

mpz_class right(mpz_class z) {
	mpz_class x, y, i;

	if(z == 0) {
		return (0);
	}

	x = 0;
	for(i=0; i<z; i++) {
		y = 0;
		for(i=0; i<z; i++) {
			if (::pair(x, y) == z) {
				return (y);
			}
			y++;
		}
		x++;
	}
	return (0);
}




/**
 * Utils
 * =================================================================
 */
mpz_class length(mpz_class a) {
	mpz_class l, b, i;

	if(a == 0) return(0);

	l = 1;
	b = a;

	for(i=0; i<a; i++) {
		if(right(b) != 0) {
			b = right(b);
			l++;
		} else {
			return (l);
		}
	}
	return(0);
}

mpz_class element(mpz_class a, mpz_class n){
	mpz_class b, j, l, i;

	b = a;
	j = 1;
	l = length(a);
	for(i=0; i<l; i++) {
		if (j == n)
			return (left(b));
		else
			b = right(b);
		j++;
	}
	return(0);
}

mpz_class replace(mpz_class a, mpz_class n, mpz_class x){
	mpz_class b, j, i;
	b = 0;
	j = length(a);
	for(i=0; i<j; i++) {
		if (j == n)
			b = ::pair(x, b);
		else
			b = ::pair(element(a, j), b);
		j--;
	}
	return(b);
}

mpz_class sequence(mpz_class x, mpz_class k) {
	mpz_class r,i;

	r = 0;
	for(i=0; i<k; i++) {
		r = ::pair(x, r);
	}
	return (r);
}





/**
 * icCode
 * =================================================================
 */


mpz_class isCode(mpz_class p) {
	mpz_class pc,k,m,S,l,max,a,b;

	if(length(p) < 3) return(0);

	k = element(p, 1);
	m = element(p, 2);
	S = element(p, 3);
	l = length(S);
	max = 0;

	if(k < m) return(0);

	pc = 1;
	while(pc < l + 1) {
	 	if(element(element(S, pc), 1) == 1) {
	 		if(length(element(S, pc)) != 2)
	 			return (0);

			a = element(element(S, pc), 2);
	 		if(a>m) return (0);
	 		if(a>l) return (0);
	 		pc++;
	 	}
	 	else if(element(element(S, pc), 1) == 2) {
	 		if(length(element(S, pc)) != 3)
	 			return (0);

			a = element(element(S, pc), 2);
			b = element(element(S, pc), 3);
	 		if(a>m) return (0);
	 		if(b>m) return (0);
	 		pc++;
	 	}
	 	else if(element(element(S, pc), 1) == 3) {
	 		if(length(element(S, pc)) != 3)
	 			return 0;

			a = element(element(S, pc), 2);
			b = element(element(S, pc), 3);
	 		if(a>m) return (0);
	 		if(b>m) return (0);
	 		pc++;
	 	}
	 	else if(element(element(S, pc), 1) == 4) {
	 		if(length(element(S, pc)) != 2)
	 			return(0);

			a = element(element(S, pc), 2);
	 		if(a>m) return (0);
	 		pc++;
	 	}
	 	else if(element(element(S, pc), 1) == 5) {
	 		if(length(element(S, pc)) != 2)
	 			return(0);

			a = element(element(S, pc), 2);
	 		if(a>m) return (0);
	 		pc++;
	 	}
	 	else if(element(element(S, pc), 1) == 6) {
	 		if(length(element(S, pc)) != 3)
	 			return(3);

			a = element(element(S, pc), 2);
			b = element(element(S, pc), 3);
			if(a>m) return (0);
			if(b>m) return (0);
			if(b>l) return (0);
	 		pc++;
	 	}
	 }
	 return (0);
}





/**
 * Executable
 * =================================================================
 */


mpz_class executable(mpz_class p, mpz_class x) {
	mpz_class ak, xk;

	if(isCode(p) == 0) return 0;

	ak = element(p, 1);
	xk = length(x);
	if(ak ==  xk) return 1;
	return 0;
}




/**
 * Comp
 * =================================================================
 */



mpz_class comp(mpz_class p, mpz_class x) {
	mpz_class pc,v,i,k,m,S,n,a,b;

	if(executable(p,x) == 1) {
		k = element(p, 1);
		m = element(p, 2);
		S = element(p, 3);
		n = length(S) + 1;

		v = sequence(0, m);
		i = 1;
		for(i=0; i<k; i++) {
			v = replace(v, i, element(x, i));
			i++;
		}

		pc = 1;

		while (pc<n){

			if (element(element(S, pc), 1) == 1) {
				pc = element(element(S, pc), 2);
			}
			else if (element(element(S, pc), 1) == 2) {
				a = element(element(S, pc), 2);
				b = element(element(S, pc), 3);
				v = replace(v,a,b);
				pc++;
			}
			else if (element(element(S, pc), 1) == 3) {
				a = element(element(S, pc), 2);
				b = element(element(S, pc), 3);
				v = replace(v, i, element(v, b));
				pc++;
			}
			else if (element(element(S, pc), 1) == 4) {
				a = element(element(S, pc), 2);
				v = replace(v, a, element(v,a) + 1);
				pc++;
			}
			else if (element(element(S, pc), 1) == 5) {
				a = element(element(S, pc), 2);
				if (element(v, a) != 0) v = replace(v, a, element(v,a) - 1);
				pc++;
			}
			else if (element(element(S, pc), 1) == 6) {
				a = element(element(S, pc), 2);
				b = element(element(S, pc), 3);
				if (element(v, a) > 0) pc = b; else pc++;
			}
		}
		return (element(v, 1));


	} else {
		while (0 == 0) {}
	}
}









/**
 * Main
 * - $ g++ -lgmp -lgmpxx -o a samples/comp/comp2.cpp -O3  -mtune=native -march=native
 * =================================================================
 */
int main(void) {
		mpz_class tashizan, arg, r;

    clock_t start = clock();

		tashizan = "213797904982138037454632940231947778583451643946214351540021648522868699116020532367794717279183597514183155623196270804266008911831504391186653218399859236746627965664600576149708185774554408383297605366764401745745512661359368506";
		arg = 19; // <2,3>

		r = comp(tashizan,arg);

    clock_t end = clock();

		cout << "result " << r << "\n";

    const double time = static_cast<double>(end - start) / CLOCKS_PER_SEC * 1000.0;
    printf("time %lf[ms]\n", time);

    return (0);
}
