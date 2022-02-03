import React, { useEffect, useState } from 'react';
import  Death_Eater  from '../../images/DE_Dashboard.jpg';
import Phoenix_Order from '../../images/PO_Dashboard.jpg';
import Crucio from '../../images/crucio.jpg';
import Imperio from '../../images/imperio.jpg';
import Expelliarmus from '../../images/castle1.jpg';
import Avada from '../../images/avada.jpg';
import Divination from '../../images/divination.jpg';
import None_po from '../../images/empty_po.jpg';
import None_de from '../../images/empty_de.jpg';
import Win_po_card from '../../images/win_po_card.jpg';
import Win_de_card from '../../images/win_de_card.jpg';

export const Piece = props => {
   
    const [img, setImg] = useState('');

    useEffect(() => {
        var card = ""
        if(props.imgSrc === 'death'){
            card = Death_Eater
        }else if(props.imgSrc === 'phoenix'){
            card = Phoenix_Order
        }else if(props.imgSrc === 'crucio'){
            card = Crucio
        }else if(props.imgSrc === 'imperio'){
            card = Imperio
        }else if(props.imgSrc === 'expelliarmus'){
            card = Expelliarmus
        }else if (props.imgSrc === 'avada'){
            card = Avada
        }else if(props.imgSrc === 'divination'){
            card = Divination
        }else if(props.imgSrc === 'win_po'){
            card = Win_po_card
        }else if(props.imgSrc === 'win_de'){
            card = Win_de_card
        }else{
            if(props.faction === 'phoenix'){
                card = None_po
            }else{
                card = None_de
            }
        }

        setImg(card);
    }, [props])
  
    return (
        <div class='column is-2'>
            <figure class="image is-64x64">
                <img  class='dashboard-card' height='64' width='64' src={ img } alt="Image" />
            </figure>
        </div>
    );

}