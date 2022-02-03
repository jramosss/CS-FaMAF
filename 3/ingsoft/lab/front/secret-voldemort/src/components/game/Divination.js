import React, { useContext, useState, useEffect } from 'react';
import  Death_Eater  from '../../images/DE_Dashboard.jpg';
import Phoenix from '../../images/PO_Dashboard.jpg';
import Cancel from '../../images/cancel.png';
import Wand from '../../images/wand.png';
import { sendRequest } from '../../services/request';
import { userContext } from '../../user-context';
import Popup from 'reactjs-popup';
import 'reactjs-popup/dist/index.css';
import '../../custom.css';
import '../../popup_custom.css';

/* This component will get the cards to discard  for minister and director */

/* PROPS_NEEDED: room_name, imgSrc, ind  */

export const Divination = (props) => {

    const context = useContext(userContext);
    const [room_name, setRoomName] = useState('')
    const [cards, setCards] = useState([])
    const [modalText, setModalText] = useState('')
    

    useEffect(() => {
       setRoomName(props.room_name)
    }, [props])

    /* A simple handle error function */
    const handleMessages = (message) => {
        let btnDivination = document.getElementById("btn-spell")
        btnDivination.click()
        let btnModal = document.getElementById("btnModalDivination")
        setModalText(message)
        btnModal.click()
    }


    /* Function that cast the spell */
    const castDivination = () => {
        const headers = {
            Accept: "application/json",
            Authorization: "Bearer " + context.token,
            "Content-Type": "application/json"
        }
        try {
            const path = "http://127.0.0.1:8000/" + room_name + "/cast/divination"
            sendRequest('GET', headers, {}, path).then(async response => {
                const data = await response.json();
                if(!response.ok){ 
                    handleMessages(data.detail)
                }else{
                    setCards(data.cards)
                }
            }).catch(error => {
                handleMessages("Ups! something went wrong.")
            })
        }catch(e){
            handleMessages("Error getting data from the current match")
        }
    }
    /* Modularization, will get the card and return the render figure with its
    proper card image */
    const getCard = (card) => {
        console.log(card)
        let image =  (card === 'Death Eater proclamation') ? (Death_Eater) : 
        ((card ==='Order of the Fenix proclamation') 
        ? (Phoenix) : Cancel);

        return(<figure class="image figure-divination is-128x128">
                    <img class="img-divination" src={ image } alt=""/>
              </figure>)
    }

    /* This function confirm that the cards were seen by the minister */ 
    const confirmDivination = () => {
        const headers = {
            Accept: "application/json",
            Authorization: "Bearer " + context.token,
            "Content-Type": "application/json"
        }
        try {
            const path = "http://127.0.0.1:8000/" + room_name + "/cast/confirm_divination"
            sendRequest('PUT', headers, {}, path).then(async response => {
                const data = await response.json();
                if(!response.ok){  // nothing will be displayed if the response is ok.
                    handleMessages(data.detail)
                }
            }).catch(error => {
                handleMessages("Ups! something went wrong.")
            })
        }catch(e){
            handleMessages("Error getting data from the current match")
        }
    }

    return (
            <div class="align-cntr">
                <Popup className='alert-modal' 
                trigger={<button id='btnModalDivination' 
                style={{display:"none"}}></button>} 
                modal position='right center'>
                        <p> 
                            {modalText}
                        </p>
                </Popup>
                <Popup className='divination-modal' 
                    trigger={
                        <button id='btn-spell' class="btn-spell">
                            <figure class="image is-64x64">
                                <img height='128' width='128' src={ Wand }
                                alt=""/>
                            </figure>
                        </button>
                    }
                     modal position='right center'
                     onOpen={castDivination}
                     > 
                    {(cards.length !== 0) ? 
                        cards.map((card, index) => (getCard(card)))
                        : <p></p>
                    }
                    <br /><button class='login-button' onClick={confirmDivination}>Ready</button>
                </Popup>
            </div>
    );
}