class expresion {
    public boolean atomic(){
        return false;
    }
    public expresion lsub(){
        return null;
    }
    public expresion rsub(){
        return null;
    }
    public int value(){
        return 0;
    }
}

class number extends expresion{
    private int num;
    expresion exp = new expression();
    public number(int n){
        this.num = n;
    }
    public boolean atomic = true;
    public expresion lsub = null;
    public expresion rsub = null;
    public int value = num;
}

class sum extends expression {
    private int left;
    private int right;
    public sum(int e1, int e2){
        this.left = e1;
        this.right = e2;
    }
    public boolean atomic = false;
    exp.lsub = left;
    
}