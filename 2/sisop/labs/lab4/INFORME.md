> Universidad Nacional de Córdoba  --  Facultad de Matemática, Astronomía, Física y Computación  --  Sistemas Operativos 2020

> Grupo Hormigas Constructoras:

> Giuliano, Maximiliano: maxigiuliano18@gmail.com

> Osiecki, Agustin: a.osiecki@mi.unc.edu.ar

> Ramos, Julian: jramostod@gmail.com


# Informe laboratorio 4: Censurando un sistemas de archivos 

## Objetivo:
Estudiar y Comprender mejor el funcionamiento de los file systems a traves de las funciones que nos provee la libreria de FUSE manejando la fat table y sus clusters.

---

## Primera Parte: La Censura
En esta primera parte estudiando y comprendiendo el codigo
modificamos la funcion `fat_fuse_read` de `fat_fuse_ops.c` para que cuando lea el buffer, reemplaze todos los caracteres de una palabra censurada (osea que exista en censorship.h) por x's.
```c
  for (u32 k = 0; k < NUMERO_DE_PALABRAS_PROHIBIDAS; k++){
    //word = una palabra del archivo que estamos leyendo
    if (strcmp(word,prohibited[k]) == 0){
      for (u32 p = 0 ; p < strlen(word); p++)
          buf[start+p] = 'x';
    }
  }
```

---

## Segunda Parte: Escribiendo nuevos clusters
En esta segunda parte nos encontramos con un nuevo desafio que de la misma manera que la primera (estudiando el codigo y comprendiendolo) modificamos la funcion `fat_file_pwrite` de `fat_file.c` para que pueda asignar clusters dinamicamente

```c
//Engañamos al sistema para que no piense que el cluster esta vacio
    fat_table_set_next_cluster(file->table,cluster,1);
    next_free_cluster = fat_table_get_next_free_cluster(file->table);
//Si quedan menos de 512 bytes a ser escritos, significa que con el
//proximo cluster ya terminamos de escribir nuestro archivo
    if (bytes_remaining < 512) {
      //Por lo que el proximo cluster deberia ser EOC
      fat_table_set_next_cluster(file->table,next_free_cluster,FAT_CLUSTER_END_OF_CHAIN);
      fat_table_set_next_cluster(file->table,cluster,next_free_cluster);
      check(errno);
    }
        //Si no nos alcanza con un solo cluster
    else {
      //Seteamos el proximo cluster como el proximo vacio en la FAT
      fat_table_set_next_cluster(file->table,cluster,next_free_cluster);
      check(errno);
    }
    cluster = next_free_cluster;
```

Esta parte nos dio varios problemas, fue complicado comprender el codigo y como estaban siendo asignados los clusters. Intentamos varias maneras de afrontar el problema, pero terminamos optando por la que definimos arriba, Cuando quedan menos de 512 bytes a ser escritos, seteamos el proximo cluster como `EOC` y tambien lo seteamos como siguiente cluster de nuestro cluster actual.

---

## Tercera Parte: Registro de Actividades

* a) En esta primera parte de la tercera parte del lab modificamos la funcion `fat_fuse_readdir` de `fat_fuse_ops.c` (ya que se llama cuando se monta el sistema) para que llame a `mknod`, creando asi nuestro archivo `.fs.log`, el "." al principio del nombre hace que el archivo sea `oculto`

```c
    //Si no existe un archivo con el nombre .fs.log
    if (!hidden_file_exists){
        //Entonces lo creamos
        fat_fuse_mknod(HIDDEN_FILE_PATH,0,0);
    }
```

* b) En esta segunda parte hicimos que tanto `fat_fuse_read` como `fat_fuse_write` llamen a `log_activity`, registrando asi cuando se hace un read o un write.

```c
//registramos cuando se hace un read
  fat_fuse_log_activity(OP_READ,file);

//registramos cuando se hace un write
  fat_fuse_log_activity(OP_WRITE,file);
```

Por ende fue facil hacer que `fat_fuse_log_activity` registrara los logs, pero no fue facil hacer que escriba, No pensamos que la solucion fuese como termino siendo.

```c
    //obtenemos el file_tree
    fat_tree tree = get_fat_volume()->file_tree;
    //obtenemos el archivo .fs.log
    fat_file fslog = fat_tree_search(tree,HIDDEN_FILE_PATH);
    //Obtenemos el directorio padre de nuestro archivo .fs.log
    fat_file parent = fat_tree_get_parent(
                fat_tree_node_search(tree,HIDDEN_FILE_PATH));
    //Escribimos el log en .fs.log
    fat_file_pwrite(fslog,buf,LOG_MESSAGE_SIZE,fs_offset,parent);
    //aumentamos el offset en la cantidad que hayamos escrito en el archivo
    fs_offset += (off_t)strlen(buf);
```

* c) Modificamos la funcion `fat_fuse_readdir` para que cuando encontremos nuestro archivo `.fs.log` hagamos que sus primeros bytes sean = 0xE5 y que sus attribs = FILE_ATTRIBUTE_SYSTEM.

```c
  //Buscamos el archivo llamado .fs.log
  if (strcmp((*child)->name,HIDDEN_FILE_NAME) == 0){
    (*child)->dentry->attribs = FILE_ATTRIBUTE_SYSTEM;
    //marcamos el archivo como "Listo para eliminarse"
    (*child)->dentry->base_name[0] = 0xe5;
  }
```

---

## Conclusion:
Este laboratorio no fue tan sencillo de encararlo sobre todo la parte 2 que costo bastante, pero nos sirvio para entender como se manejaba la fat table y sus clusters. A su vez tambien para reafirmar nuestros conocimientos aprendidos en el teorico de la materia. Requirio de mucha lectura y comprension del codigo, ya que las cosas no estaban a simple vista.