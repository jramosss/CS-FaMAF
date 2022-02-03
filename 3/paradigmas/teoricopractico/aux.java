//package aux;

import java.io.*;

interface helpers {
    double sum(double a,double b);
    double prod(double a,double b);
    double rest(double a,double b);
    double div(double a,double b) throws ArithmeticException;
}

class calc implements helpers{
    public double sum(final double a, final double b) {
        return a + b;
    }

    public double prod(final double a, final double b) {
        return a * b;
    }

    public double rest(final double a, final double b) {
        return a - b;
    }

    public double div(final double a, final double b) {
        try {
            return a / b;
        } catch (final ArithmeticException e) {
            System.out.println(e.getMessage());
            return -1;
        }
    }

    public void display (final double...ds){
        for(double d : ds){
            System.out.println(d);
        }
    }
}

class aux{
    public static void main(String[] args) {
        calc cal = new calc();
        double a = cal.sum(3,5);
        double b = cal.prod(3.1,1.2);
        double c = cal.rest(0,8);
        double d = cal.div(5,0);
        cal.display(a,b,c,d);
    }
}

