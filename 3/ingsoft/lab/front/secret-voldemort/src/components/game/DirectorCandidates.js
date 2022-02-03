import React, { useState, useEffect, useContext } from 'react';
import Popup from 'reactjs-popup';
import { sendRequest } from '../../services/request';
import { userContext } from '../../user-context';
import 'reactjs-popup/dist/index.css';
import '../../custom.css';
import '../../popup_custom.css';

export function DirectorCandidates(props) {

  const [room_name, setRoomName] = useState('')
  const [name, setName] = useState('')
  const [players, setPlayers] = useState([])
  const [last_director, setLastDirector] = useState('')
  
  const context = useContext(userContext)

  useEffect(() => {
    setRoomName(props.room_name)
    setName(props.name)
    setLastDirector(props.last_director)
    setPlayers(props.players)
  }, [props]);

  const handleMessage = (message) => {
    let btnModalDir = document.getElementById("btnModal_director")
    btnModalDir.click()
    let p_director = document.getElementById("p_director")
    btnModalDir.click()
    p_director.innerText = message
  }
  
  const handleSelection = (e) => {
    const selectedUser = e.target.name;
    const keys = {director_uname: selectedUser};
    const authorizationToken = "Bearer " + context.token;
    const header = {
      Accept: "application/json",
      Authorization: authorizationToken,
      "Content-Type": "application/json"
    };
    const path = `http://127.0.0.1:8000/${room_name}/director`
    
    sendRequest("PUT", header, keys, path).then(async response => {
      const data = await response.json();

      if (response.ok){
        console.log(data.message);
      } else {
        handleMessage(data.detail)
      }
    }).catch(error => handleMessage(error));
  }

  return(
  <div>
    <Popup className='alert-modal' trigger={<button id='btnModal_director' style={{display:"none"}}></button>} modal position='right center'>
      <p id='p_director'> 
      </p>
    </Popup>
    <Popup className='director-modal' trigger={<button class='panel-button'>Propose Director</button>} modal position='right center' >
      {close =>
        <div class='container has-text-centered'>
          <ul>
            {
              players.map((user) => 
                ((user !== name) && (user !== last_director)) ?
                  <li class='i-playerlist'> {user} 
                    <button class='panel-button mx-3 my-3' name={user} onClick={handleSelection} onClickCapture={close}>
                        Select
                    </button>
                  </li>
                :
                  <li class='i-playerlist'>
                    {user} (X)
                  </li>
              )
            }
          </ul>
        </div>
      }
    </Popup>
  </div>
  );
}
