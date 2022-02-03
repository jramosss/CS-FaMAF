import React, { useEffect, useState } from "react";
import { sendRequest } from "./../../services/request";
import { ChaosPiece } from "./ChaosPiece";
import dolores from "../../images/dolores.png"; 

// PROPS: token, room_name, chaos


// CORREGIR:  Centrar en tablero, contador en 0 => no mostrar, must be centered vert betwen dash y panel

//Display chaos counter and cast it
export const ChaosCounter = props =>{

  const [Images, setImages] = useState([])
  
  useEffect(() => {
    const count = props.chaos
    var array = [];
    for(let i = 0; i < count; i++){
      array.push(dolores);
    }

    setImages(array);
  }, [props])

  return(
    
    props.chaos === 0 ? <div></div> :
    
    <div class='container'>
      <div class='column is-6 is-offset-3 align-cntr is-vcentered'>
        <div class='chaos-card'>
          <div class='card-content'>
            <div class='columns align-cntr is-vcentered'>
              {(props.chaos > 0 ? 
                <div class="column is-3">
                  <p class='chaos-text has-text-centered'>
                    Chaos              
                  </p>
                </div>
                : <div></div>
              )}
              { 

                Images.map( image =>
                  <ChaosPiece image={image}/>
                )              
                
              
                
              }
            </div>
          </div>
        </div>
      </div>
    </div>
    
  );
}
