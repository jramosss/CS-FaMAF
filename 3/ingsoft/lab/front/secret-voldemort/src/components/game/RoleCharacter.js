import React, { useEffect, useState} from 'react';
import '../../stylesheet/custom.css';
import  Phoenix  from '../../images/po.jpg';
import Death_Eater from '../../images/de.jpg';
import Voldemort from '../../images/voldemort.jpg';

export const RoleCharacter = (props) => {

    const [image,setImage] = useState('')

    useEffect(()=> {
        var image = '';
        (props.role === 'Death eater') ? (image = Death_Eater) : 
        ((props.role === 'Member of the Fenix Order') ? 
        (image = Phoenix) : image = Voldemort)
        setImage(image)
    },[props])

    return(
        <div class="container align-cntr">
            <p class='panel-title is-medium'>Role</p>
            <figure class="image is-128x128 fig-inline">
                <img height='128' width='128' src={image} alt="" />
            </figure>
        </div>
    )
}

