#include <iostream>

using namespace std;
/*
//7.12
/*
class Persona
{
    private :
        string m_strNombre;
    int m_nEdad ;
    bool m_bEsVaron ;
    public :
    Persona ( std :: string strNombre , int nEdad , bool bEsVaron )
    : m_strNombre ( strNombre ) , m_nEdad ( nEdad ) ,m_bEsVaron ( bEsVaron ){}
    std :: string GetNombre () { return m_strNombre ; }
    int GetEdad () { return m_nEdad ; }
    bool EsVaron () { return m_bEsVaron ; }
};
class Empleado {
    private :
    std :: string m_strEmpleador ;
    double m_dSalario ;
    public :
    Empleado ( std :: string strEmpleador , double dSalario )
    : m_strEmpleador ( strEmpleador ) ,
    m_dSalario ( dSalario )
    {
    }
    std :: string GetEmpleador () { return m_strEmpleador ; }
    double GetSalario () { return m_dSalario ; }
};

class Profesor : public Persona , public Empleado
{
    private :
        int m_nDictaGrado; 
    public :
        Profesor ( std :: string strNombre , int nEdad , bool bEsVaron ,
        std :: string strEmpleador , double dSalario , int
        nDictaGrado )
        : Persona ( strNombre , nEdad , bEsVaron ) ,
        Empleado ( strEmpleador , dSalario ) ,m_nDictaGrado ( nDictaGrado )
        {
        }
};


//7.13

class Persona{
    private:
        std::string m_strNombre;
        int m_nEdad;
        bool m_bEsVaron;
    public:
        Persona(std::string strNombre, int nedad, bool bEsVaron)
        : m_strNombre(strNombre), m_nEdad(nEdad).
        m_bEsVaron(bEsVaron){}
        std::string GetNombre() {return m_strNombre;}
        int GetEdad(){return m_nEdad;}
        bool EsVaron(){return m_bEsVaron;}
};

class Empleado{
    private:
        std::string m_strEmpleador;
        double m_dSalario;
    public :
        Empleado ( std :: string strEmpleador , double dSalario )
        : m_strEmpleador ( strEmpleador ) ,m_dSalario ( dSalario ){}
        std :: string GetEmpleador () { return m_strEmpleador ; }
        double GetSalario () { return m_dSalario ; }
}

class Profesor : public Persona , public Empleado
{
    private :
        int m_nDictaGrado ;
    public :
        Profesor ( std :: string strNombre , int nEdad , bool
        bEsVaron ,std :: string strEmpleador , double dSalario , int nDictaGrado )
        : Persona ( strNombre , nEdad , bEsVaron ) ,Empleado ( strEmpleador , dSalario ) ,
        m_nDictaGrado ( nDictaGrado )
        {
        }
};
*/

class Felino {
    public:
        virtual void meow() = 0;
};

class Gato : public Felino {
    public:
        void meow(){
            std::cout << "miau\n";
        }
};

class Tigre : public Felino {
    public:
        void meow(){
            std::cout << "ROARR\n";
        }
};

class Ocelote : public Felino {
    public:
        void meow(){
            std::cout << "roarr\n";
        }
};

int main(int argc, char const *argv[]){
    //Felino f = new Felino();
    //Felino.meow();
    auto g = new Gato();
    g->meow();
    return 0;
}

