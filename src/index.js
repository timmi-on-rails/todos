import { Elm } from "./Main.elm";

async function init() {
    let webDavConfig = {
        url: localStorage.getItem('webdav-url'),
        user: "",
        password: ""
    };

    try {
        const credentials = await navigator.credentials.get({ password: true });
        webDavConfig.user = credentials.id;
        webDavConfig.password = credentials.password;
    } catch (e) {
        console.log(e);
    }

    const app = Elm.Main.init({
        node: document.getElementById("root"),
        flags: webDavConfig
    });

    app.ports.setWebDavConfig.subscribe(async function (state) {
        localStorage.setItem('webdav-url', state.url);

        const cred = new PasswordCredential({
            id: state.user,
            password: state.password,
        });

        try {
            await navigator.credentials.store(cred);
        } catch (e) {
            console.log(e);
        }
    });
}

init();
