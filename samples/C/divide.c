fn divide(x, y) {
    int minus, z, i;

    if ((x==0) || (y==0))
        return(0);

    minus = 0;
    if ((x<0) && (y>0)) {
        x = -x;
        minus = 1;
    } else if ((x>0) && (y<0)) {
        y = -y;
        minus = 1;
    } else if ((x<0) && (y<0)) {
        x = -x;
        y = -y;
    }

    z = 0;
    i = 1;
    loop(x) {
        z = add(z, y);
        if (z == x) {
            if (minus == 1)
                i = -i;
            return(i);
        }
        if (z > x) {
            i--;
            if (minus == 1)
                i = -i;
            return(i);
        }
    i++;
    }

}