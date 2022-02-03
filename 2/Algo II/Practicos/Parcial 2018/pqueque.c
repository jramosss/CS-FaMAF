type pqueue = heap

proc empty(out q:pqueue)
q.size:= 0
end
{Post: q ∼ Vacía}

{Pre: q ∼ Q}
fun is_empty(q:pqueue) ret b:bool
b:= (q.size = 0)
end
{Post: b ∼ es_vacía Q}

{Pre: q ∼ Q ∧ q.size < n}
proc enqueue(in/out q:pqueue,in e:elem)
q.size:= q.size+1
q.elems[q.size]:= e
float(q)
end
{Post: q ∼ Encolar Q e}

{Pre: q ∼ Q ∧¬is_empty(q)}
fun first(q:pqueue) ret e:elem
e:= q.elems[1]
end
{Post: e ∼ primero Q}

{Pre: q ∼ Q ∧¬is_empty(q)}
proc dequeue(in/out q:pqueue)
q.elems[1]:= q.elems[q.size]
q.size:= q.size-1
sink(q)
end
{Post: q ∼ decolar Q}

