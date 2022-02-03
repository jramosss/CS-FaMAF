/* First, the standard lib includes, alphabetically ordered */
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>


/* Maximum allowed length of the array */
#define MAX_SIZE 100000

void print_help(char *program_name) {
    /* Print the usage help of this program. */
    printf("Usage: %s <input file path>\n\n"
           "Sort an array given in a file in disk.\n"
           "\n"
           "The input file must have the following format:\n"
           " * The first line must contain only a positive integer,"
           " which is the length of the array.\n"
           " * The second line must contain the members of the array"
           " separated by one or more spaces. Each member must be an integer."
           "\n\n"
           "In other words, the file format is:\n"
           "<amount of array elements>\n"
           "<array elem 1> <array elem 2> ... <array elem N>\n\n",
           program_name);
}

char *parse_filepath(int argc, char *argv[]) {
    /* Parse the filepath given by command line argument. */
    char *result = NULL;

    if (argc < 2) {
        print_help(argv[0]);
        exit(EXIT_FAILURE);
    }

    result = argv[1];

    return (result);
}

unsigned int array_from_file(int array[],
           unsigned int max_size,
           const char *filepath) {  	
      FILE *file;
      file=fopen(filepath , "r"); 
      
      unsigned int length;
      
      fscanf(file,"%u",&length);
      
      unsigned int n;  
      
      if (length> max_size) {
             n=max_size;}
      else { n=length;}
      
      for (unsigned int i=0;i<n; i++) {
       fscanf(file,"%d",&array[i]); 
}     
      fclose(file);             
        
return 0;

}

void array_dump(int a[], unsigned int length) {
    printf("[");
    for (unsigned int i=0;i<length;i++){
     printf("%d",a[i]);
} 
    printf("]");
}


int main(int argc, char *argv[]) {
    char *filepath = NULL;


	printf("nombre del ejecutable %s\n", argv[0]);
	printf("nombre del archivo con el arreglo %s\n", argv[1]);
        

    /* parse the filepath given in command line arguments */
    filepath = parse_filepath(argc, argv);

    
    /* create an array of MAX_SIZE elements */
    int array[MAX_SIZE];
    
    /* parse the file to fill the array and obtain the actual length */
    unsigned int length = array_from_file(array, MAX_SIZE, filepath);
    

    /*dumping the array*/
    array_dump(array, length);
    
    return (EXIT_SUCCESS);
}
