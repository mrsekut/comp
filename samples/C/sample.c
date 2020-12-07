int main(void) {
    printf("%d", hoge(2,3));
    return (0);
}


fn hoge(x, y) {
    z = x + y + 1;
    return (z);
}

fn hoge(x, y) {
    z = add(add(x, y), 1)
    return (z);
}

fn hoge(x, y) {
    w1 = add(x, y)
    z = add(w1, 1)
    return (z);
}



// 原子条件式の制限, 四則演算の関数化, 関数のインライン展開
fn hoge(x, y) {
    int v0, v1, r, a, w1, z;

    // add(x, y)
    v0 = x;					// 引数分の初期化
    v1 = y;					// 同じ
    r = 0;					// return分の初期化
    a = v1;
    a++;
    if (a > 0) {
        loop(v1)
            v0++;
        r = v1;				// addのreturn部分をrにしている
    } else {
        v1 = -v1;
        loop(v1)
            v0--;
        r = v1;				// addのreturun部分をrにしている
    }
    w1 = r;

    // add(_, 1)
    v0 = w1;				// 引数分の初期化
    v1 = 1;					// 同じ
    r = 0;					// return分の初期化
    a = v1;
    a++;
    if (a > 0) {
        loop(v1)
            v0++;
        r = v1;				// addのreturn部分をrにしている
    } else {
        v1 = -v1;
        loop(v1)
            v0--;
        r = v1;				// addのreturun部分をrにしている
    }

    z = r;
    return (z);
}


// 自然数チェック
fn hoge(x, y) {
    int v0, v1, r, a, w1, z, sig_v0, sig_v1, sig_a, sig_w1, sig_z, sig_x, sig_y, sig_r, i;

    sig_v0 = 1;
    sig_v1 = 1;
    sig_r = 1;
    sig_a = 1;
    sig_w1 = 1;
    sig_z = 1;
    sig_x = 1;
    sig_y = 1;

    v0 = x;
    sig_v0 = sig_x;
    v1 = y;
    sig_v1 = sig_y;
    r = 0;
    sig_r = 1;
    a = v1;
    sig_a = sig_v1;
    if (sig_a > 0)
        a++;
    else {
        if (a > 0)
            a--;			// 本にはx--'とあるが、x--でいいはず
        else {				// ここ入ることあるのか？
            a = 1;
            sig_a = 1;
        }
    }


    if ((a > 0) && (sig_a > 0)) {
        loop(v1) {
            if (sig_v0 > 0)
                v0++;
            else {
                if (v0 > 0)
                    v0--;			// 本にはx--'とあるが、x--でいいはず
                else {				// ここ入ることあるのか？
                    v0 = 1;
                    sig_v0 = 1;
                }
            }
        }
        r = v0;				// addのreturn部分をrにしている
        sig_r = sig_v1;
    } else {
        if(sig_v1 > 0)
            sig_v1 = 0;
        else
            sig_v1 = 1;
        loop(v1) {
            if (sig_v0 > 0) {
                if (v0 > 0)
                    v0--;				// 本にはx--'とあるが、x--でいいはず
                else {
                    v0 = 1;
                    sig_v0 = 0;
                }
            } else
                v0++;
        }
        r = v0;				// addのreturun部分をrにしている
        sig_r = sig_v0;
    }
    w1 = r;
    sig_w1 = sig_r;


    // add(_, 1)
    v0 = w1;
    sig_v0 = sig_w1;
    v1 = 1;
    sig_v1 = 1;
    r = 0;
    sig_r = 1;
    a = v1;
    sig_a = sig_v1;
    if (sig_a > 0)
        a++;
    else {
        if (a > 0)
            a--;			// 本にはx--'とあるが、x--でいいはず
        else {				// ここ入ることあるのか？
            a = 1;
            sig_a = 1;
        }
    }

    if ((a > 0) && (sig_a > 0)) {
        loop(v1) {
            if (sig_v0 > 0)
                v0++;
            else {
                if (v0 > 0)
                    v0--;			// 本にはx--'とあるが、x--でいいはず
                else {				// ここ入ることあるのか？
                    v0 = 1;
                    sig_v0 = 1;
                }
            }
        }
        r = v0;				// addのreturn部分をrにしている
        sig_r = sig_v0;
    } else {
        if(sig_v1 > 0)
            sig_v1 = 0;
        else
            sig_v1 = 1;
        loop(v1) {
            if (sig_v0 > 0) {
                if (v0 > 0)
                    v0--;				// 本にはx--'とあるが、x--でいいはず
                else {
                    v0 = 1;
                    sig_v0 = 0;
                }
            } else
                v0++;
        }
        r = v0;				// addのreturun部分をrにしている
        sig_r = sig_v0;
    }

    z = r;
    sig_z = sig_r;
    return (z);
}


// loopの除去
fn hoge(x, y) {
    int v0, v1, r, a, w1, z, sig_v0, sig_v1, sig_a, sig_w1, sig_z, sig_x, sig_y, sig_r, i, f;

    sig_v0 = 1;
    sig_v1 = 1;
    sig_r = 1;
    sig_a = 1;
    sig_w1 = 1;
    sig_z = 1;
    sig_x = 1;
    sig_y = 1;

    v0 = x;
    sig_v0 = sig_x;
    v1 = y;
    sig_v1 = sig_y;
    r = 0;
    sig_r = 1;
    a = v1;
    sig_a = sig_v1;
    if (sig_a > 0)
        a++;
    else {
        if (a > 0)
            a--;			// 本にはx--'とあるが、x--でいいはず
        else {				// ここ入ることあるのか？
            a = 1;
            sig_a = 1;
        }
    }


    if ((a > 0) && (sig_a > 0)) {
        f = v1;
        while(f > 0) {
            if (sig_v0 > 0)
                v0++;
            else {
                if (v0 > 0)
                    v0--;			// 本にはx--'とあるが、x--でいいはず
                else {				// ここ入ることあるのか？
                    v0 = 1;
                    sig_v0 = 1;
                }
            }
            f--;
        }
        r = v0;				// addのreturn部分をrにしている
        sig_r = sig_v1;
    } else {
        if(sig_v1 > 0)
            sig_v1 = 0;
        else
            sig_v1 = 1;
        f = v1;
        while(f > 0) {
            if (sig_v0 > 0) {
                if (v0 > 0)
                    v0--;				// 本にはx--'とあるが、x--でいいはず
                else {
                    v0 = 1;
                    sig_v0 = 0;
                }
            } else
                v0++;
            f--;
        }
        r = v0;				// addのreturun部分をrにしている
        sig_r = sig_v0;
    }
    w1 = r;
    sig_w1 = sig_r;


    // add(_, 1)
    v0 = w1;
    sig_v0 = sig_w1;
    v1 = 1;
    sig_v1 = 1;
    r = 0;
    sig_r = 1;
    a = v1;
    sig_a = sig_v1;
    if (sig_a > 0)
        a++;
    else {
        if (a > 0)
            a--;			// 本にはx--'とあるが、x--でいいはず
        else {				// ここ入ることあるのか？
            a = 1;
            sig_a = 1;
        }
    }

    if ((a > 0) && (sig_a > 0)) {
        f = v1;
        while(f >  0) {
            if (sig_v0 > 0)
                v0++;
            else {
                if (v0 > 0)
                    v0--;			// 本にはx--'とあるが、x--でいいはず
                else {				// ここ入ることあるのか？
                    v0 = 1;
                    sig_v0 = 1;
                }
            }
            f--;
        }
        r = v0;				// addのreturn部分をrにしている
        sig_r = sig_v0;
    } else {
        if(sig_v1 > 0)
            sig_v1 = 0;
        else
            sig_v1 = 1;
        f = v1;
        while(f > 0) {
            if (sig_v0 > 0) {
                if (v0 > 0)
                    v0--;				// 本にはx--'とあるが、x--でいいはず
                else {
                    v0 = 1;
                    sig_v0 = 0;
                }
            } else
                v0++;
            f--;
        }
        r = v0;				// addのreturun部分をrにしている
        sig_r = sig_v0;
    }

    z = r;
    sig_z = sig_r;
    return (z);
}



// whileの除去, returnの制限
int hoge(int x,int y) {
    int v0, v1, r, a, w1, z, sig_v0, sig_v1, sig_a, sig_w1, sig_z, sig_x, sig_y, sig_r, i, f, rst, sig_rst;

    sig_v0 = 1;
    sig_v1 = 1;
    sig_r = 1;
    sig_a = 1;
    sig_w1 = 1;
    sig_z = 1;
    sig_x = 1;
    sig_y = 1;
    sig_rst = 1;

    v0 = x;
    sig_v0 = sig_x;
    v1 = y;
    sig_v1 = sig_y;
    r = 0;
    sig_r = 1;
    a = v1;
    sig_a = sig_v1;
    if (sig_a > 0)
        a++;
    else {
        if (a > 0)
            a--;			// 本にはx--'とあるが、x--でいいはず
        else {				// ここ入ることあるのか？
            a = 1;
            sig_a = 1;
        }
    }


    if ((a > 0) && (sig_a > 0)) {
        f = v1;
        L1:
            if(f > 0) {
                if (sig_v0 > 0)
                    v0++;
                else {
                    if (v0 > 0)
                        v0--;			// 本にはx--'とあるが、x--でいいはず
                    else {				// ここ入ることあるのか？
                        v0 = 1;
                        sig_v0 = 1;
                    }
                }
                f--;
                goto L1;
            }
        r = v0;				// addのreturn部分をrにしている
        sig_r = sig_v1;
    } else {
        if(sig_v1 > 0)
            sig_v1 = 0;
        else
            sig_v1 = 1;
        f = v1;
        L2:
            if(f > 0) {
                if (sig_v0 > 0) {
                    if (v0 > 0)
                        v0--;				// 本にはx--'とあるが、x--でいいはず
                    else {
                        v0 = 1;
                        sig_v0 = 0;
                    }
                } else
                    v0++;
                f--;
                goto L2;
            }
        r = v0;				// addのreturun部分をrにしている
        sig_r = sig_v0;
    }
    w1 = r;
    sig_w1 = sig_r;


    // add(_, 1)
    v0 = w1;
    sig_v0 = sig_w1;
    v1 = 1;
    sig_v1 = 1;
    r = 0;
    sig_r = 1;
    a = v1;
    sig_a = sig_v1;
    if (sig_a > 0)
        a++;
    else {
        if (a > 0)
            a--;			// 本にはx--'とあるが、x--でいいはず
        else {				// ここ入ることあるのか？
            a = 1;
            sig_a = 1;
        }
    }

    if ((a > 0) && (sig_a > 0)) {
        f = v1;
        L3:
            if(f >  0) {
                if (sig_v0 > 0)
                    v0++;
                else {
                    if (v0 > 0)
                        v0--;			// 本にはx--'とあるが、x--でいいはず
                    else {				// ここ入ることあるのか？
                        v0 = 1;
                        sig_v0 = 1;
                    }
                }
                f--;
                goto L3;
            }
        r = v0;				// addのreturn部分をrにしている
        sig_r = sig_v0;
    } else {
        if(sig_v1 > 0)
            sig_v1 = 0;
        else
            sig_v1 = 1;
        f = v1;
        L4:
            if(f > 0) {
                if (sig_v0 > 0) {
                    if (v0 > 0)
                        v0--;				// 本にはx--'とあるが、x--でいいはず
                    else {
                        v0 = 1;
                        sig_v0 = 0;
                    }
                } else
                    v0++;
                f--;
                goto L4;
            }
        r = v0;				// addのreturun部分をrにしている
        sig_r = sig_v0;
    }

    z = r;
    sig_z = sig_r;
    rst = z;
    goto L;

L:
    return(rst);
}

// if文をgotoに変換
int hoge(int x,int y) {
    int v0, v1, r, a, w1, z, sig_v0, sig_v1, sig_a, sig_w1, sig_z, sig_x, sig_y, sig_r, i, f, rst, sig_rst;

L0:
    sig_v0 = 1;
    sig_v1 = 1;
    sig_r = 1;
    sig_a = 1;
    sig_w1 = 1;
    sig_z = 1;
    sig_x = 1;
    sig_y = 1;
    sig_rst = 1;
    v0 = x;
    sig_v0 = sig_x;
    v1 = y;
    sig_v1 = sig_y;
    r = 0;
    sig_r = 1;
    a = v1;
    sig_a = sig_v1;
    if (!(sig_a > 0)) goto L7;
    a++;
    if(!(a > 0)) goto L5;
    a--;			// 本にはx--'とあるが、x--でいいはず
    goto  L6;
L5:
    a = 1;
    sig_a = 1;
L6:
L7:
    if (!((a > 0) && (sig_a > 0))) goto L71;
    f = v1;
L1:
    if(!(f > 0)) goto L10;
    if (!(sig_v0 > 0)) goto L11;
    v0++;
    goto L12;
L11:
    if (!(v0 > 0)) goto L111;
    v0--;
    goto L112;
L111:
    v0 = 1;
    sig_v0 = 1;
L112:
L12:
    f--;
    goto L1;
L10:
    r = v0;				// addのreturn部分をrにしている
    sig_r = sig_v1;
    goto L72;
L71:
    if(!(sig_v1 > 0)) goto L101;
    sig_v1 = 0;
    goto L102;
L101:
    sig_v1 = 1;
L102:
    f = v1;
L2:
    if(!(f > 0)) goto L21;
    if (!(sig_v0 > 0)) goto L211;
    if (!(v0 > 0)) goto L2111;
    v0--;
    goto L2112;
L2111:
    v0 = 1;
    sig_v0 = 0;
L2112:
    goto L212;
L211:
    v0++;
L212:
    f--;
    goto L2;
L21:
    r = v0;				// addのreturun部分をrにしている
    sig_r = sig_v0;
L72:
    w1 = r;
    sig_w1 = sig_r;
    // add(_, 1)
    v0 = w1;
    sig_v0 = sig_w1;
    v1 = 1;
    sig_v1 = 1;
    r = 0;
    sig_r = 1;
    a = v1;
    sig_a = sig_v1;
    if (!(sig_a > 0)) goto L50;
    a++;
    goto L51;
L50:
    if (!(a > 0)) goto L501;
    a--;
    goto L502;
L501:
    a = 1;
    sig_a = 1;
L502:
L51:
    if (!((a > 0) && (sig_a > 0))) goto L301;
    f = v1;
L3:
    if(!(f >  0)) goto L31;
    if (!(sig_v0 > 0)) goto L311;
    v0++;
    goto L312;
L311:
    if (!(v0 > 0)) goto L3111;
    v0--;
    goto L3112;
L3111:
    v0 = 1;
    sig_v0 = 1;
L3112:
L312:
    f--;
    goto L3;
L31:
    r = v0;
    sig_r = sig_v0;
    goto L302;
L301:
    if(!(sig_v1 > 0)) goto L401;
    sig_v1 = 0;
    goto L402;
L401:
    sig_v1 = 1;
L402:
L4:
    if(!(f > 0)) goto L41;
    if (!(sig_v0 > 0)) goto L411;
    if (!(v0 > 0)) goto L4111;
    v0--;
    goto L4112;
L4111:
    v0 = 1;
    sig_v0 = 0;
L4112:
    goto L412;
L411:
    v0++;
L412:
    f--;
    goto L4;
L41:
    r = v0;				// addのreturun部分をrにしている
    sig_r = sig_v0;
L302:
    z = r;
    sig_z = sig_r;
    rst = z;
    goto L;
L:
    return(rst);
}


// 条件式の分解
int hoge(int x,int y) {
    int v0, v1, r, a, w1, z, sig_v0, sig_v1, sig_a, sig_w1, sig_z, sig_x, sig_y, sig_r, i, f, rst, sig_rst;

L0:
    sig_v0 = 1;
    sig_v1 = 1;
    sig_r = 1;
    sig_a = 1;
    sig_w1 = 1;
    sig_z = 1;
    sig_x = 1;
    sig_y = 1;
    sig_rst = 1;
    v0 = x;
    sig_v0 = sig_x;
    v1 = y;
    sig_v1 = sig_y;
    r = 0;
    sig_r = 1;
    a = v1;
    sig_a = sig_v1;
    if (!(sig_a > 0)) goto L7;
    a++;
    if(!(a > 0)) goto L5;
    a--;			// 本にはx--'とあるが、x--でいいはず
    goto  L6;
L5:
    a = 1;
    sig_a = 1;
L6:
L7:
    if (!(a > 0)) goto L701;
    goto L702;
L701:
    if (!(sig_a > 0)) goto L71;
L702:
    f = v1;
L1:
    if(!(f > 0)) goto L10;
    if (!(sig_v0 > 0)) goto L11;
    v0++;
    goto L12;
L11:
    if (!(v0 > 0)) goto L111;
    v0--;
    goto L112;
L111:
    v0 = 1;
    sig_v0 = 1;
L112:
L12:
    f--;
    goto L1;
L10:
    r = v0;				// addのreturn部分をrにしている
    sig_r = sig_v1;
    goto L72;
L71:
    if(!(sig_v1 > 0)) goto L101;
    sig_v1 = 0;
    goto L102;
L101:
    sig_v1 = 1;
L102:
    f = v1;
L2:
    if(!(f > 0)) goto L21;
    if (!(sig_v0 > 0)) goto L211;
    if (!(v0 > 0)) goto L2111;
    v0--;
    goto L2112;
L2111:
    v0 = 1;
    sig_v0 = 0;
L2112:
    goto L212;
L211:
    v0++;
L212:
    f--;
    goto L2;
L21:
    r = v0;				// addのreturun部分をrにしている
    sig_r = sig_v0;
L72:
    w1 = r;
    sig_w1 = sig_r;
    // add(_, 1)
    v0 = w1;
    sig_v0 = sig_w1;
    v1 = 1;
    sig_v1 = 1;
    r = 0;
    sig_r = 1;
    a = v1;
    sig_a = sig_v1;
    if (!(sig_a > 0)) goto L50;
    a++;
    goto L51;
L50:
    if (!(a > 0)) goto L501;
    a--;
    goto L502;
L501:
    a = 1;
    sig_a = 1;
L502:
L51:
    if (!(a > 0)) goto L30101;
    goto L30102;
L30101:
    if (!(sig_a > 0)) goto L301;
L30102:
    f = v1;
L3:
    if(!(f >  0)) goto L31;
    if (!(sig_v0 > 0)) goto L311;
    v0++;
    goto L312;
L311:
    if (!(v0 > 0)) goto L3111;
    v0--;
    goto L3112;
L3111:
    v0 = 1;
    sig_v0 = 1;
L3112:
L312:
    f--;
    goto L3;
L31:
    r = v0;
    sig_r = sig_v0;
    goto L302;
L301:
    if(!(sig_v1 > 0)) goto L401;
    sig_v1 = 0;
    goto L402;
L401:
    sig_v1 = 1;
L402:
L4:
    if(!(f > 0)) goto L41;
    if (!(sig_v0 > 0)) goto L411;
    if (!(v0 > 0)) goto L4111;
    v0--;
    goto L4112;
L4111:
    v0 = 1;
    sig_v0 = 0;
L4112:
    goto L412;
L411:
    v0++;
L412:
    f--;
    goto L4;
L41:
    r = v0;				// addのreturun部分をrにしている
    sig_r = sig_v0;
L302:
    z = r;
    sig_z = sig_r;
    rst = z;
    goto L;
L:
    return(rst);
}
