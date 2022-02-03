import React, { useState, useEffect, useContext, memo } from 'react';
import '../../stylesheet/custom.css';
import { Card } from './Card';
import { userContext } from '../../user-context';
import { sendRequest } from '../../services/request';
import Popup from 'reactjs-popup';

/* PROPS_NEEDED:  room_name, minister, director, phase */
function DiscardPanel(props) {
    
    const context = useContext(userContext);
    const [room_name, setRoomName] = useState('');
    const [minister, setMinister] = useState('');
    const [director, setDirector] = useState('');
    const [de_procs, setDEprocs] = useState('');
    const [phase, setPhase] = useState(-1);
    const [cards, setCards] = useState('');

    useEffect(() => {
        setRoomName(props.room_name);
        setMinister(props.minister);
        setDirector(props.director);
        setPhase(props.phase);
        setDEprocs(props.de_procs);
        let btnOnModal = document.getElementById("btnOnModal")
        btnOnModal.click()
    },[props]);

    // Getting into the server for card getting
    const  getCards = () => {
        const headers = {
            Accept: "application/json",
            Authorization: "Bearer " + context.token,
            "Content-Type": "application/json"
        };
        const room = room_name;
        const path = "http://127.0.0.1:8000/" + room.toString() +"/cards";
        sendRequest('GET', headers, {}, path).then(async response => {
            const data = await response.json();
            console.log("Aca va la response perrix: " + data.cards);
            if(!response.ok){ 
               console.log(data.detail.toString());
            }else{ 
                console.log(data.cards.toString())
                setCards(data.cards.toString());
            }
        }).catch(error => {
            console.log("There was an error at" + path.toString());
        })
    };
    // Depending on phase and if its a minister or director will get cards (or not)
     const showCards = async () => {
        if(phase === 3 && (context.username === minister)){
            await getCards()
            return true
        } else if(phase === 4 && (context.username === director)){
            await getCards()
            return true
        } else if(phase === 12 && (context.username === director)){
          await getCards()
          return true
        }
        return false
    };

    return(
      <Popup  className='divination-modal' 
              closeOnDocumentClick={false} 
              closeOnEscape={false}
              modal position='right center'
              trigger={<button id='btnOnModal' style={{display:"none"}}></button>}>
        {(close) => (
        <div class='container align-cntr my-6 py-3'> 
          <p class='game-title align-cntr'>Discard</p>
          <div class="columns">
            {   
            (director === context.username && phase === 12) ?
              showCards() === false ? console.log(cards.split(','))
                : (
                  cards.split(',').map((card, index) =>
                  <div class='column is-4 align-cntr'> 
                    <Card ind={index} room_name={room_name}
                      image={card} />
                  </div>)
                  )
            : (
              (director === context.username && de_procs >= 5) ?
                showCards() === false ? console.log("never")
                  : (
                    /* Insert one more index since the Expelliarmus was enabled */
                    cards.concat(',expelliarmus').split(',').map((card, index) =>
                    <div class='column is-4 align-cntr'>
                    <Card ind={(index === 2) ? (index+1) : index} room_name={room_name}
                      image={card} />
                    </div>)
                    )
              : (
                showCards() === false ? console.log("never")
                  : (
                    cards.split(',').map((card, index) =>
                    <div class='column is-4 align-cntr'> 
                      <Card ind={index} room_name={room_name}
                        image={card} />
                    </div>)
                    )
                )
            )
            }   
          </div>
        </div>
        )}
      </Popup>
    )
}

export const MemoizedDiscardPanel = memo(DiscardPanel)