using System;

namespace aux {
    public class Expression{
        public Expression(){
            Console.WriteLine("La verdad no se\n");
        }
        public bool atomic(){
            return !subexp;
        }
        public Expression lsub(){
            if(!atomic){
                return left;
            }
        }
        public Expression lsub(){
            if(!atomic){
                return right;
            }
        }
        public int value = 0;
    }
    public class Number : Expression {
        private var num;
        public Number(int n){
            this.num = n;
        }
        exp.atomic = false;
        exp.lsub = null;
        exp.rsub = null;
        exp.value = num;
    }
    public class Sum : Expression {
        private var left;
        private var right;
        public Sum(var e1,var e2){
            this.left = e1;
            this.right = e2;
        }
        public bool atomic = false;
        exp.lsub = left;
        exp.rsub = right;
        exp.value = left.value + right.value;
    }//Ejercicio a:
    public class Product : Expression{
        private var left;
        private var right;
        public Product(var e1,var e2){
            this.left = e1;
            this.right = e2;
        }
        public bool atomic = false;
        exp.lsub = left;
        exp.rsub = right;
        exp.value = left.value * right.value;
    }
    /*Ejercicio b:
        val a = number(3); -> Llama a number(Number class)
        val b = number(5); -> Llama a number(Number class)
        val c = number(7); -> Llama a number(Number class)
        val d = sum(a,b);  -> Llama a Sum(), devuelve 8
        val e = prod(d,c); -> Llama a Product(), devuelve 56
    */
    public class Pow : Expression { //c
        private Expression base; //Esta mal usar base ya que es un keyword, pero ya que esto no me esta compilando cual hay
        private Expression exponent;
        public Pow(Expression base, Expression exponent){
            this.base = base;
            this.exponent = exponent;
        }
        Expression exp = new Expression();
        exp.atomic = false;
        exp.lsub = base;
        exp.rsub = exponent;
        exp.value = Math.Pow(base.value,exponent.value);
    }
    public class Question : Expression{ //d
        private Expression b;
        public Question(Expression b){
            this.b = b;
        }
        exp.atomic = false;
        exp.lsub = b;
        exp.rsub = null;
        exp.value = b.value;
    }
}

namespace ej2 {
    enum shape_tag {s_point, s_circle, s_rectangle};
    class point{
        shape_tag tag;
        int x;
        int y;
        point (int xval, int yval) { 
            x = xval;
            y = yval;
            tag = s point;
        }
        int x_coord (){ return x;}
        int y_coord (){ return y;}
        void move (int dx, int dy){ x += dy; y += dy;}
    }
    class circle{
        shape_tag tag;
        point c;
        int r;
        circle (point center, int radius) {
             c = center;
             r = radius;
             tag = s_circle
        }
        point center (){ return c;}
        int radius (){ return radius;}
        void move (int dx, int dy){ c.move (dx, dy);}
        void stretch (int dr){ r += dr;}
    }
    
    class rectangle{
        shape_tag tag;
        point tl;
        point br;
        rectangle (point topleft, point botright)
        { tl = topleft; br = botright; tag = s_rectangle;}
        point top_left (){ return tl;}
        point bot_right (){ return br;}
        void move (int dx, int dy){ tl.move (dx, dy); br.move (dx, dy);}
        void stretch (int dx, int dy){ br.move (dx, dy);}
        /* Rotate shape 90 degrees. */
        void rotate (void *shape){
            switch ((shape_tag *) shape){
                case s_point:
                    int d = ((rect->bot_right().x_coord ()-rect->top_left().x_coord()) -(rect->top_left().y_coord ()
                    -rect->bot_right().y_coord()));
                    rect->move (d, d);
                    rect->stretch (-2.0 * d, -2.0 * d);
                    break;
                case s_rectangle:
                    rectangle *rect = (rectangle *)
                    shape;
                    int d = ((rect->bot_right().x_coord ()-rect->top_left().x_coord()) -(rect->top_left().y_coord ()-rect->bot_right().y_coord()));
                    rect->move (d, d);
                    rect->stretch (-2.0 * d, -2.0 * d);
                    break;
                }
        }
    }
}