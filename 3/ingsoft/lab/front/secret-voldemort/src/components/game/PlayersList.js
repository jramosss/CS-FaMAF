import React, { useState, useEffect } from 'react';
import '../../stylesheet/custom.css';

export const PlayersList = (props) => {
    const [players, setPlayers] = useState([])

    useEffect(() => {
        setPlayers(props.players)
    },[props])

    return( <div class="container align-cntr">
                <p class='panel-title'>Players list</p>
                <ul class='player-list'>
                    {players.map((player) => 
                        <li class='i-playerlist'>{player}</li>
                    )}
                </ul>
            </div>)
}