#include "types.h"
#include "user.h"

void 
fondo(void)
{
  for (int i = 0;i<320;i++){             
      for(int j=0;j<200;j++)
      {
          plotpixel(i,j,00);
      }
  }

}

void
printsquare (int x, int y, int color, int linesize)
{
    for (int i = x; i < x + linesize; i++){
        for (int j = y; j < y + linesize; j++)
            plotpixel(i,j,color);
    }
}

void
vertical_line(int x, int y, int length, int color, int linesize)
{
    for (int j = y; j < y + length; j++)
        printsquare(x,j,color,linesize);
}

void
horizontal_line(int x, int y, int length, int color, int linesize)
{
    for (int i = x; i < x + length; i++)
        printsquare(i,y,color,linesize);
   
}

void
diagonal_dec(int x, int y, int length, int color, int linesize)
{
    int k = 0; 
      while (k < length){
          printsquare(x+k,y+k,color,linesize);
          k++;
      }
}

void
diagonal_inc(int x, int y, int length, int color, int linesize)
{
    int k = 0;
      while (k < length){                                       
         printsquare(x+k,y-k,color,linesize);                   
         k++;                                                   
      }
 
}

void
letraD(int x, int y,int color, int linesize)
{
    vertical_line(x,y,25,color,linesize);
    diagonal_dec(x+3,y,13,color,linesize); //x+3 por el ancho de printsquare
    diagonal_inc(x+3,y+24,11,color,linesize);
}

void
letraE(int x, int y,int color, int linesize)
{
    vertical_line(x,y,25,color,linesize);
    horizontal_line(x+3,y,13,color,linesize);
    horizontal_line(x+3,y+12,13,color,linesize);
    horizontal_line(x+3,y+24,13,color,linesize);
}

void
letraS(int x, int y,int color, int linesize)
{
    horizontal_line(x+3,y,13,color,linesize);
    vertical_line(x,y,11,color,linesize);
    horizontal_line(x,y+12,15,color,linesize);
    vertical_line(x+15,y+12,11,color,linesize);
    horizontal_line(x,y+24,15,color,linesize);
}

void
letraT(int x, int y, int color, int linesize)
{
    horizontal_line(x,y,25,color,linesize);
    vertical_line(x+12,y+1,25,color,linesize);
}

void
letraR(int x, int y, int color, int linesize)
{
    horizontal_line(x,y,15,color,linesize);
    vertical_line(x,y+1,13,color,linesize);
    vertical_line(x+15,y+1,13,color,linesize);
    horizontal_line(x,y+13,15,color,linesize);
    vertical_line(x,y+13,11,color,linesize);
    diagonal_dec(x,y+13,13,color,linesize);
}

void
letraO(int x, int y, int color, int linesize)
{
    horizontal_line(x,y,15,color,linesize);
    vertical_line(x,y+1,25,color,linesize);
    vertical_line(x+15,y+1,25,color,linesize);
    horizontal_line(x,y+25,15,color,linesize);
}

void
letraY(int x, int y, int color, int linesize)
{
    diagonal_dec(x-2,y,10,color,linesize);
    diagonal_inc(x+7,y+9,10,color,linesize);
    vertical_line(x+7,y+12,15,color,linesize);
}

void
letraA(int x, int y, int color, int linesize)
{
    horizontal_line(x+3,y,13,color,linesize);
    vertical_line(x,y+1,25,color,linesize);
    vertical_line(x+16,y+1,25,color,linesize);
    horizontal_line(x+3,y+13,13,color,linesize);
}

void
letraL(int x, int y, int color, int linesize)
{
    vertical_line(x,y,25,color,linesize);
    horizontal_line(x,y+25,17,color,linesize);
}

void
firstline(void)
{
    int size = 4;
    letraD(20,40,4,size);
    sleep(50);
    letraE(60,40,4,size);
    sleep(20);
    letraS(100,40,4,size); 
    sleep(20);
    letraT(140,40,4,size);
    sleep(20);
    letraR(190,40,4,size);
    sleep(20);
    letraO(230,40,4,size);
    sleep(20);
    letraY(270,40,4,size);
    sleep(50);
}

void
secondline(void)
{
  int size = 4;
    letraA(100,90,4,size);
    sleep(30);
    letraL(140,90,4,size);
    sleep(30);
    letraL(180,90,4,size);
    sleep(30);

}

void
thirdline(void)
{ 
  letraO(120,140,4,4);
  letraS(165,140,4,4);
  printsquare(145,162,4,4);
  printsquare(145,165,4,4);
  printsquare(190,162,4,4);
  printsquare(190,165,4,4);
}

void
warning_sign(int x, int y,int color)
{
    diagonal_dec(x+4,y,10,color,4);
    diagonal_inc(x-6,y+10,10,color,4);
    horizontal_line(x-2,y+6,10,color,4);
    horizontal_line(x-6,y+10,20,color,4);
    horizontal_line(x-2,y+14,10,color,4);
    diagonal_dec(x-6,y+10,10,color,4);
    diagonal_inc(x+4,y+20,10,color,4);
    vertical_line(x+4,y+4,8,00,4);
    printsquare(x+4,y+16,00,4);
}

int
main(void)
{
    modeswitch(1);
    fondo();
  
    firstline();
    secondline();
    thirdline();
    sleep(50);
    // struct spinlock* lck;
    //initlock(lck,"candado");
    // acquire(lck);
    // void * ptr = (void*) 15;
    while (1){ 
        warning_sign (20,100,14);
        warning_sign (60,100,14);
        warning_sign (230,100,14);
        warning_sign (270,100,14);
        sleep(70);
        warning_sign (20,100,00);
        warning_sign (60,100,00);
        warning_sign (230,100,00);
        warning_sign (270,100,00);
        sleep(70);
    }
    return 0;
}