import React, { useState, useEffect, memo } from 'react';
import Popup from 'reactjs-popup';
import 'reactjs-popup/dist/index.css';
import { sendRequest } from '../../services/request';
import { VotesList } from './VotesList';

/* TODO: Need to add the list of users and polling to obtain the votes of the 
 * other players.
 * TODO: Although i need to disable or close the popup when i click one time 
 */


/* PROPS NEEDED: roomname, usersVotes, token */ 
function Vote(props) {
  
  const [room_name, setRoomName] = useState('');
  const [usersVotes, setUsersVotes] = useState([]);
  const [token, setToken] = useState('');
  const [director, setDirector] = useState('');
  const [minister, setMinister] = useState('');

  useEffect(() => {
    setRoomName(props.room_name)
    setUsersVotes(props.usersVotes)
    setToken(props.token)
    setDirector(props.director)
    setMinister(props.minister)
    let btnModalVote = document.getElementById("btnModalVote")
    btnModalVote.click()
  },[props]);

  console.log("LAS PROPS O EL STATE: " + room_name + " " + usersVotes + " " + token)
  
  const handleVote = (e) => {
    const authorizationToken = "Bearer " + token
    const vote = e.target.name;
    const keys = {"vote": vote}
    const header = {
      Accept: "application/json",
      Authorization: authorizationToken,
      "Content-Type": "application/json"
    };
    const path = `http://127.0.0.1:8000/${room_name}/vote`

    /* Send the vote decision */
    sendRequest("PUT", header, keys, path).then(async response => {
      const data = await response.json();
      if (response.ok) {
        console.log("Vote succesfully");
        let btnModalVote = document.getElementById("btnModalVote")
        btnModalVote.click()
      } else {
        console.log("Error detail: " + data.detail.json());
      }
    }).catch(error => {
      console.log("There was an error on voting");
    });
  }

  return(
      <Popup className='divination-modal'
            trigger={<button id='btnModalVote' style={{display:"none"}}></button>}
            closeOnDocumentClick={false} 
            closeOnEscape={false}
            modal 
            position='right center'
      >
        {(close) => ( 
            (usersVotes.length == 0)  ? (
              <div class='container has-text-centered'>
                <h3 class='room-title'>{minister} proposed {director} for being the next director</h3>
                <button class='panel-button is-medium mx-3' name='Lumos' onClick={(e) => handleVote(e)} /*onClickCapture={close}*/ >Lumos</button>
                <button class='panel-button is-medium mx-3' name='Nox' onClick={(e) => handleVote(e)} /*onClickCapture={close}*/ >Nox</button>
              </div>) 
              : (<VotesList usersVotes={usersVotes}/>)
          )
      }
      </Popup>
  )
}

export const MemoizedVote = memo(Vote, (prev, next) => {
      return prev.usersVotes.toString() === next.usersVotes.toString()
});