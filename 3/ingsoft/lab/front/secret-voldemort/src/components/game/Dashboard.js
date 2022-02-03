import React, { useState, useEffect } from 'react';
import '../../stylesheet/custom.css';
import { Piece } from './Piece';

/* This component will show the proclamations for each team*/
/* PROPS_NEEDED: proclam_de, proclam_op */
export const Dashboard = (props) => {
    const [proclam_de, setProclamDe] = useState(-1)
    const [proclam_op, setProclamOp] = useState(-1)
    const [n_of_players, setNumberOfPlayers] = useState(5)

    useEffect(() => {
        setProclamDe(props.proclam_de);
        setProclamOp(props.proclam_op);
        setNumberOfPlayers(props.n_of_players)
    }, [props])

    /* Function that depends on the faction. */ 
    const getPieces = (img) => {
        var cantProclam = 0
        var dashboard_de = []
        var components = ['none','none','none','none','none','none']
        // Preparing dashboard for the death eaters
        if(n_of_players === 5 || n_of_players === 6){
            dashboard_de = ["none", "none", "divination", "avada", "avada","win_de"]
        }
        if(n_of_players === 7 || n_of_players === 8){
            dashboard_de = ["none", "crucio", "imperio", "avada", "avada","win_de"]
        }
        if(n_of_players === 9 || n_of_players === 10){
            dashboard_de = ["crucio", "crucio", "imperio", "avada", "avada","win_de"]
        }
        /* Setting the correct image for each proclamation */
        if(img === 'death'){
            cantProclam = proclam_de - 1 
            components = dashboard_de
        }else {
            cantProclam = proclam_op - 1
            components.shift()
            components[4] = "win_po"
        }
        for(var i=0; i <= cantProclam; i++){
            components[i] = img
        }

        return components;
    }

    return (
        <div class="container dashboard-bg">
            <div class='container px-5'>
                <div class="columns is-vcentered">
                    <div class='column is-12'>
                        <div class='columns'>
                            {
                                getPieces('death').map( (name) => 
                                <Piece imgSrc = {name} faction='death'/>
                                )
                            }
                        </div>
                    </div>
                </div>
                <br/>
                <br/>
                <div class="columns">
                    <div class='column is-12'>
                        <div class='columns'>
                        {
                            getPieces('phoenix').map( name =>
                                <Piece imgSrc = { name } faction='phoenix' />
                            )
                        }
                        </div>
                    </div>
                </div>
            </div>
        </div>
    )
}