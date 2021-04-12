var ethereumJsUtil = require('ethereumjs-utils');
var locationCheck = require('./dualLocationCheck.js');
require('@metamask/legacy-web3');
const {
    requestAccounts,
    bscImport,
    handleWalletEvents,
    getWallet,
    sendTransaction,
} = require("./metamask.js")
const chains = require("../../config.json");


const COOKIE_CONSENT = "cookie-consent";
const basePath = new URL(document.baseURI).pathname;

const { Elm } = require('../../elm/App.elm');

window.addEventListener('load', function () {
    const app = startDapp();

    gtagPortStuff(app);

    app.ports.log.subscribe((x) => console.log(x));

    app.ports.bscImport.subscribe((_) =>
        bscImport()
            .then(app.ports.chainSwitchResponse.send)
            .catch(app.ports.chainSwitchResponse.send)
    );

    app.ports.beginLocationCheck.subscribe(function (data) {
        console.log(locationCheck);
        locationCheck.dualLocationCheckWithCallback(app.ports.locationCheckResult.send);
    });

    app.ports.connectToWeb3.subscribe(() =>
        (async () => {
            const [account] = await requestAccounts();

            const wallet = account ? await getWallet(account) : null;

            app.ports.walletResponse.send(wallet);
        })().catch((e) => {
            app.ports.walletResponse.send(e);
        })
    );

    app.ports.txSend.subscribe((params) =>
        sendTransaction(params)
            .then(app.ports.txSendResponse.send)
            .catch(app.ports.txSendResponse.send)
    );

    // app.ports.refreshWallet.subscribe((account) =>
    //     (async () => {
    //         const balance = await getBalance(account);

    //         app.ports.balanceResponse.send(balance);
    //     })().catch(app.ports.balanceResponse.send)
    // );
});

function startDapp() {
    const hasWallet = Boolean(window.ethereum);

    const app = Elm.App.init({
        node: document.getElementById('elm'),
        flags: {
            basePath: basePath,
            width: window.innerWidth,
            height: window.innerHeight,
            nowInMillis: Date.now(),
            cookieConsent: getCookieConsent(),
            chains,
            hasWallet,
        }
    });

    web3PortStuff(app, web3);


    if (hasWallet) {
        handleWalletEvents(app.ports.walletResponse.send);
    }

    return app;
}

function web3PortStuff(app, web3) {
    //prepareWeb3PortsPreConnect(app, web3);

    // web3.eth.getAccounts(function (e, res) {
    //     if (res && res.length > 0) {
    //         connectAndPrepareRemainingWeb3Ports(app, web3);
    //     }
    // });

    app.ports.web3Sign.subscribe(function (data) {
        web3.personal.sign(data.data, data.address, function (err, res) {
            var response = {
                address: data.address,
                pollId: data.pollId,
                pollOptionId: data.pollOptionId,
                sig: res
            };
            app.ports.web3SignResult.send(response)
        });
    });

    app.ports.web3ValidateSig.subscribe(function (data) {
        const id = data.id;
        const signedResponse = data.data;
        const sig = data.sig;
        const givenAddress = data.address;

        const msgBuffer = ethereumJsUtil.toBuffer(signedResponse);
        const msgHash = ethereumJsUtil.hashPersonalMessage(msgBuffer);
        const signatureBuffer = ethereumJsUtil.toBuffer(sig);
        const signatureParams = ethereumJsUtil.fromRpcSig(signatureBuffer);
        const publicKey = ethereumJsUtil.ecrecover(
            msgHash,
            signatureParams.v,
            signatureParams.r,
            signatureParams.s
        );
        const addressBuffer = ethereumJsUtil.publicToAddress(publicKey);
        const recoveredAddress = ethereumJsUtil.bufferToHex(addressBuffer);

        const success = (recoveredAddress == givenAddress);
        var successObject = {
            id: id,
            success: success
        };

        app.ports.web3ValidateSigResult.send(successObject);
    });
}

function gtagPortStuff(app) {
    app.ports.gTagOutPort.subscribe(function (data) {
        gtag('event', data.event, {
            'event_category': data.category,
            'event_label': data.label,
            'value': data.value
        });
    });

    app.ports.consentToCookies.subscribe(function () {
        setCookieConsent();
    });
}

function getCookieConsent() {
    return Boolean(window.localStorage.getItem(COOKIE_CONSENT))
}
function setCookieConsent() {
    window.localStorage.setItem(COOKIE_CONSENT, true)
}