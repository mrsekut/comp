fn add(x, y) {
    if (y >= 0) {
        loop(y)
            x++;
        return(x);
    } else {
        y = -y;
        loop(y)
            x--;
        return(x);
    }
}