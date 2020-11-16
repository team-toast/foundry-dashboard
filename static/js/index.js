var elm_ethereum_ports = require('elm-ethereum-ports');
var networkChangeNotifier = require('./networkChangeNotifier');
var ethereumJsUtil = require('ethereumjs-utils');
var locationCheck = require('./dualLocationCheck.js');

require('@metamask/legacy-web3');
require('jquery');

const { web3 } = window;

import { Elm } from '../../elm/App'

const basePath = new URL(document.baseURI).pathname;

//window.testStuff = secureComms.testStuff;
window.web3Connected = false;

window.addEventListener('load', function () {
    startDapp();
});

function startDapp() {
    if (typeof web3 !== 'undefined') {
        web3.version.getNetwork(function (e, networkId) {
            var id;
            if (e) {
                console.log("Error initializing web3: " + e);
                id = 0; // 0 indicates no network set by provider
            }
            else {
                id = parseInt(networkId);
            }
            window.app = Elm.App.init({
                node: document.getElementById('elm'),
                flags: {
                    basePath: basePath,
                    networkId: id,
                    width: window.innerWidth,
                    height: window.innerHeight,
                    nowInMillis: Date.now(),
                }
            });

            gtagPortStuff(app);
            locationCheckPortStuff(app);
            web3PortStuff(app, web3);

        });
    } else {
        window.app = Elm.App.init({
            node: document.getElementById('elm'),
            flags: {
                basePath: basePath,
                networkId: 0, // 0 indicates no network set by provider
                width: window.innerWidth,
                height: window.innerHeight,
                nowInMillis: Date.now(),
            }
        });

        gtagPortStuff(app);
        locationCheckPortStuff(app);

        console.log("Web3 wallet not detected.");
    }
}

function web3PortStuff(app, web3) {
    prepareWeb3PortsPreConnect(app, web3);

    web3.eth.getAccounts(function (e, res) {
        if (res && res.length > 0) {
            connectAndPrepareRemainingWeb3Ports(app, web3);
        }
    });

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
    app.ports.gTagOut.subscribe(function (data) {
        gtag('event', data.event, {
            'event_category': data.category,
            'event_label': data.label,
            'value': data.value
        });
    });
}

function prepareWeb3PortsPreConnect(app, web3) {
    networkChangeNotifier.startWatching(app.ports.networkSentryPort, web3);

    app.ports.connectToWeb3.subscribe(function (data) {
        connectAndPrepareRemainingWeb3Ports(app, web3);
    });
}

function connectAndPrepareRemainingWeb3Ports(app, web3) {
    if (window.ethereum && !window.web3Connected) {
        window.web3 = new Web3(ethereum);
    }

    elm_ethereum_ports.txSentry(app.ports.txOut, app.ports.txIn, web3);
    elm_ethereum_ports.walletSentry(app.ports.walletSentryPort, web3);
    networkChangeNotifier.startWatching(app.ports.networkSentryPort, web3);

    if (window.ethereum && !window.web3Connected) {
        ethereum.enable();
        window.web3Connected = true;
    }
}

function locationCheckPortStuff(app) {
    app.ports.beginLocationCheck.subscribe(function (data) {
        console.log(locationCheck);
        locationCheck.dualLocationCheckWithCallback(app.ports.locationCheckResult.send);
    });
}