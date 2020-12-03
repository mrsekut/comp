fn times (x, y) {
    int z;

    z = 0;
    if (y >= 0) {
        loop(y)
            z = add(z, x);
        return(z);
    } else {
        y = -y;
        loop(y)
            z = add(z, x);
        z = -z;
        return(z);
    }
}