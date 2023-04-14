class ['a] priority_queue =
  object (_) 

  val mutable queue : 'a Queue.t = Queue.create ()

  method get_queue () = queue
  method pop () = Queue.pop queue
  method top () = Queue.top queue

  method push ?(comparator = (>)) (new_value: 'a) = 
    if(Queue.length queue = 0) then (
      Queue.push new_value queue;
    ) else (
      let rec push_aux (queue_temp: 'a Queue.t) (element: 'a) = 
        try 
            if(comparator (Queue.top queue) element) then (
              Queue.push element queue_temp;
              push_aux queue_temp (Queue.pop queue);
            ) else (
              Queue.push (Queue.pop queue) queue_temp;
              push_aux queue_temp element;
            )
        with Queue.Empty -> (
          Queue.push element queue_temp;
          queue <- queue_temp;
        )
      in push_aux (Queue.create ()) new_value;
    )
  end;;
