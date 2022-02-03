/* 
  To be able to run these tests it is necessary to install certain dependencies: 
      npm install cypress --save-dev 
   To run these tests use the command and select the test: 
      npm run test:integration
*/
context('Create a new room', () => {
    describe('Accessing the register page', () => {
        it("Try to access the register page it should display an h1", () => {
            cy.visit("http://localhost:3000/registerPage")
            cy.get("h1").contains("User registration")
        })
    })

    describe("Create an user with empty fields", () => {
        it("Will try to click on the create button and it shouldn't register", () => {
            cy.visit("http://localhost:3000/registerPage")
            cy.get("input[id=regBtn]").click()
            /* Should display the next div */ 
            cy.get("#allEmptyValid").contains("* There are empty fields")
        })
    })

    describe("Create an user with only nickname", () => {
        it("Will try to create with nickname estebanquito, souldn't register", () => {
            cy.visit("http://localhost:3000/registerPage")
            cy.get("#nameUser").type("estebanquito")
            cy.get("input[id=regBtn]").click()
            /* Should display the next div */ 
            cy.get("#allEmptyValid").contains("* There are empty fields")
        })
    })

    describe("Create an user with nickname and password should not register", () => {
        it("Password without standards", () => {
            cy.visit("http://localhost:3000/registerPage")
            cy.get("#nameUser").type("estebanquito")
            cy.get("#passUser").type("estebanquito")
            cy.get("input[id=regBtn]").click()
            /* Should display the next div */ 
            cy.get("#pswValid")
            .contains("* The password must contain at least one number and one capital letter")
        })
    })

    describe("Passwords aren't equal, nickname estebanquito should not register", () => {
        it("Differents passwords", () => {
            cy.visit("http://localhost:3000/registerPage")
            cy.get("#nameUser").type("estebanquito")
            cy.get("#passUser").type("Esteban7")
            cy.get("#passUser2").type("Esteban3")
            cy.get("input[id=regBtn]").click()
            /* Should display the next div */ 
            cy.get("#pswValid").contains("* The passwords are not equal")
            cy.get("#psw2Valid").contains("* The passwords are not equal")
        })
    })

    describe("Email bad format", () =>{
        it("The format of the email is not correct without @ should not register", () => {
            cy.visit("http://localhost:3000/registerPage")
            cy.get("#nameUser").type("estebanquito")
            cy.get("#passUser").type("Esteban7")
            cy.get("#passUser2").type("Esteban7")
            cy.get("#mailUser").type("sad")
            cy.get("input[id=regBtn]").click()
            /* Should display the next div */ 
            cy.get("#emailValid").contains("* You must select a valid e-mail")
        })
    })

    describe("Email bad format 2", () =>{
        it("The format of the email is not correct with @ should not register", () => {
            cy.visit("http://localhost:3000/registerPage")
            cy.get("#nameUser").type("estebanquito")
            cy.get("#passUser").type("Esteban7")
            cy.get("#passUser2").type("Esteban7")
            cy.get("#mailUser").type("sad@")
            cy.get("input[id=regBtn]").click()
            /* Should display the next div */ 
            cy.get("#emailValid").contains("* You must select a valid e-mail")
        })
    })

    describe("All but password are full", () => {
        it("The password input is empty should not register", () => {
            cy.visit("http://localhost:3000/registerPage")
            cy.get("#nameUser").type("estebanquito")
            cy.get("#passUser2").type("Esteban3")
            cy.get("#mailUser").type("esteban_ae@hotmail.com")
            cy.get("input[id=regBtn]").click()
            /* Should display the next div */ 
            cy.get("#allEmptyValid").contains("* There are empty fields")
        })
    })

    describe("All but nickname are full", () => {
        it("The nickname input is empty should not register", () => {
            cy.visit("http://localhost:3000/registerPage")
            cy.get("#passUser").type("Esteban7")
            cy.get("#passUser2").type("Esteban7")
            cy.get("#mailUser").type("esteban_ae@hotmail.com")
            cy.get("input[id=regBtn]").click()
            /* Should display the next div */ 
            cy.get("#allEmptyValid").contains("* There are empty fields")
        })
    })

    describe("Create user with email already in use", () => {
        it("The email is already in use should not register", () => {
            cy.visit("http://localhost:3000/registerPage")
            cy.get("#nameUser").type("esteban7")
            cy.get("#passUser").type("Esteban7")
            cy.get("#passUser2").type("Esteban7")
            cy.get("#mailUser").type("player0@example.com")
            cy.get("input[id=regBtn]").click()
            /* Should display the next div */ 
            cy.get("#allEmptyValid").contains("* Email aready registered")
        })
    })

    describe("Create user with nick already in use", () => {
        it("The email is already in use should not register", () => {
            cy.visit("http://localhost:3000/registerPage")
            cy.get("#nameUser").type("player0")
            cy.get("#passUser").type("Esteban7")
            cy.get("#passUser2").type("Esteban7")
            cy.get("#mailUser").type("player43@example.com")
            cy.get("input[id=regBtn]").click()
            /* Should display the next div */ 
            cy.get("#allEmptyValid").contains("* Username already registered")
        })
    })

    describe("Nickname length < 3", () => {
        it("The nickname length < 3 should not register", () => {
            cy.visit("http://localhost:3000/registerPage")
            cy.get("#nameUser").type("es")
            cy.get("#passUser").type("Esteban7")
            cy.get("#passUser2").type("Esteban7")
            cy.get("#mailUser").type("player99@example.com")
            cy.get("input[id=regBtn]").click()
            /* Should display the next div */ 
            cy.get("#nickValid")
            .contains("* The nickname must contain between 8 and 15 characters")
        })
    })

    describe("Nickname length > 15", () => {
        it("The nickname length > 15 should not register", () => {
            cy.visit("http://localhost:3000/registerPage")
            cy.get("#nameUser").type("estebanquito1233321123")
            cy.get("#passUser").type("Esteban7")
            cy.get("#passUser2").type("Esteban7")
            cy.get("#mailUser").type("player0@example.com")
            cy.get("input[id=regBtn]").click()
            /* Should display the next div */ 
            cy.get("#nickValid")
            .contains("* The nickname must contain between 8 and 15 characters")
        })
    })

    describe("Password length > 54", () => {
        it("The password length > 54 should not register", () => {
            cy.visit("http://localhost:3000/registerPage")
            cy.get("#nameUser").type("estebanquito")
            cy.get("#passUser").
            type("Estebanquito123332112311233212312312312313123123123123123123123123123123111111111111111111111111111111111111111")
            cy.get("#passUser2")
            .type("Estebanquito123332112311233212312312312313123123123123123123123123123123111111111111111111111111111111111111111")
            cy.get("#mailUser").type("player0@example.com")
            cy.get("input[id=regBtn]").click()
            /* Should display the next div */ 
            cy.get("#pswValid")
            .contains("* The password must contain between 8 and 54 characters")
        })
    })

    describe("Password length < 8", () => {
        it("The password length < 8 should not register", () => {
            cy.visit("http://localhost:3000/registerPage")
            cy.get("#nameUser").type("steb")
            cy.get("#passUser").type("Steb7")
            cy.get("#passUser2").type("Steb7")
            cy.get("#mailUser").type("player0@example.com")
            cy.get("input[id=regBtn]").click()
            /* Should display the next div */ 
            cy.get("#pswValid")
            .contains("* The password must contain between 8 and 54 characters")
        })
    })

    describe("Cancel button is rendered", () => {
        it("Cancel should be rendered on page", () => {
            cy.visit("http://localhost:3000/registerPage")
            cy.get("input[value=Cancel]")
        })
    })

    describe("Cancel register", () => {
        it("Press cancel button, sould render home page", () => {
            cy.visit("http://localhost:3000/registerPage")
            cy.get("#cancelBtn").click()
            cy.wait(500)
            cy.get('h1').should("be.visible").and("contain","Pytherin Project")
            cy.get('input[type=submit]').should("be.visible").and("contain", "Login")
        })
    })

    // Never test end to end, to seek for the response
})