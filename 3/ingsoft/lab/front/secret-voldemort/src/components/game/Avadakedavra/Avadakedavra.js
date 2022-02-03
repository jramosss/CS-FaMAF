import React from 'react';
import Popup from 'reactjs-popup';
import { sendRequest } from '../../../services/request';
import Wand from '../../../images/wand.png' 
// PROPS {phase, room_name, players, minister, token, email}
export const Avadakedavra = props => {

  const handleSpeell = target => {

    const header = {
      Accept: "application/json",
      Authorization: "Bearer " + props.token,
      "Content-Type": "application/json"
    }

    const keys = {
      target_uname: target
    }
    
    const path = `http://localhost:8000/${props.room_name}/cast/avada-kedavra`

    sendRequest("PUT", header, keys, path).then(async response => {

      const data = await response.json();

      if (response.ok) {
        console.log("Avadakedavra succ: " + data.detail);
      } else {
        console.log("Avadakedavra failed: " + data.detail);
      }
    }).catch(error => console.log("there was an error: " + error.detail));
  }

  return (
      <Popup 
        trigger={
          <button id='btn-spell' class="btn-spell">
            <figure class="image is-64x64">
                <img height='128' width='128' src={ Wand }
                alt=""/>
            </figure>
          </button>
        } modal position='right center'>
        
        {close => 
          <div class='container has-text-centered panel-bg'>
            <p id='title' class='panel-title'> Choose who to cast Avadakedavra on </p>
            <div class='column is-6 is-offset-3 align-cntr is-vcentered'>
              <ul>
                {
                  props.players.map(player =>
                  player !== props.minister ?
                    <li id={'i-' + player}class='i-payerlist'>
                      {player} 
                      <button id={'cast-' + player} class='room-button my-2 mx-2' 
                        onClick={() => handleSpeell(player)} 
                        onClickCapture={close}>Kill</button>
                    </li>
                  :
                    <div></div>
                  
                  )
                }
              </ul>
            </div>
          </div>
        }
      </Popup>
  );
}