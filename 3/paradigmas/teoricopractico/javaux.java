class Ejemplo1 {
    public static void main(String args[]) {
        try {
            System.out.println("primera sentencia del bloque try");
            int num = 45 / 0;
            System.out.println(num);
        } catch (ArrayIndexOutOfBoundsException e) { // Esto crashearia, ya que esta excepcion no es la que va a existir
                                                     // en este programa
            System.out.println(" ArrayIndexOutOfBoundsException ");
        } finally {
            System.out.println(" bloque finally ");
        }
        System.out.println(" fuera del bloque try - catch - finally ");
    }
}

class Ejemplo2 {
    public static void main(String args[]) {
        try {
            System.out.println("primera sentencia del bloque try ");
            int num = 45 / 0;
            System.out.println(num);
        } catch (ArithmeticException e) { // Esto se cumple, no se puede dividir por 0
            System.out.println(" ArithmeticException"); // Va a printear esto el programa
        } finally {
            System.out.println(" bloque finally ");
        }
        System.out.println(" fuera del bloque try - catch - finally ");
    }
}

class Ejemplo3 {
    public static void main ( String args []) {
        try { //Este va a ndar bien
            System.out.println ( " primera sentencia del bloque try " ) ;
            int num =45/3;
            System.out.println( num );
        }
        catch ( ArrayIndexO utOfBoundsException e ) {
            System.out . println ( " ArrayIndexOutOfBoundsException " ) ;
        }
        finally {
            System.out.println ( " bloque finally " ) ;
        }
        System.out.println ( " fuera del bloque try - catch - finally " ) ;
    }
}

class Ejercicio2 {

    public static void main (String args[]){
        ITransaccion transaccion = null;
        try {
            transaccion = sesion.EmpiezaTransaccion();
            // hacer algo
            transaccion.Commit();
        }
        catch {
            if (transaccion != null ) {
                transaccion.RestaurarEstadoPrevio () ;
            }
            throw;
        }
    }
}

public class RepartirComensales {
    public static void main(String[] args) {
        Mesa mesa = new Mesa(1);
        try {
            System.out.println(" vamos a llenar la mesa 1 ... ");
            mesa.aniadirComensal(" Ana ");
            mesa.aniadirComensal(" Juan ");
            mesa.aniadirComensal(" Maria ");
            mesa.aniadirComensal(" Pedro ");
            mesa.aniadirComensal(" Juana ");
            mesa.aniadirComensal(" Esteban ");
            mesa.aniadirComensal(" Lola "); // 7
        } catch (ExceptionMesaLlena e) {
            System.out.println("Mesa llena \n");
        } finally {
            System.out.println("La mesa ya esta completa \n");
        }
    }
}

public class Mesa {
    private int numeroDeComensales;
    private int numeroDeMesa;

    public Mesa(int numeroDeMesa) {
        this.numeroDeMesa = numeroDeMesa;
    }

    public void aniadirComensal(string comensal) throws ExcepcionMesaLlena {
        if (numeroDeComensales > 5) {
            throw new ExcepcionMesaLlena(numeroDeComensales);
        } else {
            numeroDeComensales += 1;
        }
    }
}

public class CuentaBancaria {
    private double balance;
    private int numero;

    public CuentaBancaria(int numero) {
        this.numero = numero;
    }

    public void depositar(double cantidad) {
        balance += cantidad;
    }

    public void retirar(double cantidad) throws ExcepcionFondosInsuficientes {
        if (cantidad <= balance) {
            balance -= cantidad;
        } else {
            double necesita = cantidad - balance;
            throw new ExcepcionFondosInsuficientes(necesita);
        }
    }

    public double getBalance() {
        return balance;
    }

    public int getNumber() {
        return numero;
    }
}

public class BankDemo {
    public static void main ( String [] args )
    {
        CuentaBancaria c = new CuentaBancaria (101) ;
        try{
            System . out . println ( " Depositando $500 ... " ) ;
            c . depositar (500.00) ;
            System . out . println ( " \nRetirando $100 ... " ) ;
            c . retirar (100.00) ;
            System . out . println ( " \nRetirando $600 ... " ) ;
            c . retirar (600.00) ;
        }
        catch(ExceptionFondosInsuficientes e){
            System.out.println("Te quedaste corto, necesitas: " + CuentaBancaria.necesita + " mas");
        }
        finally{
            System.out.println("Desea depositar/retirar mas dinero? <d/r/n>: \n");
            eskere;
        }
    }
}

class inherit {
    public void print(String message){
        System.out.println(message);
    }
}

class inherit2 extends inherit{
    public float pi = 3.14;
}

class inherit3 extends inherit2{
    public static void main(){
        var i = new inherit();
        i.print("dou\n");
    }
}

//7.13

class Persona{
    private String m_strNombre;
    private int m_nEdad;
    private boolean m_bEsVaron;
    public Persona(String m_strNombre,int nedad,bool bEsVaron)
    : m_strNombre(strNombre), m_nEdad(nEdad), m_bEsVaron(bEsVaron){}
    public String GetNombre() {return m_strNombre;}
    public int GetEdad(){return m_nEdad;}
    public boolean EsVaron(){return m_bEsVaron;}
}

class Empleado {
    private String m_strEmpleador;
    private double m_dSalario;
    public Empleado(String strEmpleador,double m_dSalario)
    :m_strEmpleador(strEmpleador),m_dSalario(m_dSalario){}
    public String GetEmpleador(){return m_strEmpleador;}
    public double GetSalario(){return m_dSalario;}
}

class Profesor extends Persona
{
    //No se puede hacer lo mismo
    //por que java no tiene herencia multiple
}
