{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HomePage where

import Foundation
import Yesod.Core

-- Defines a handler for the home page for any master site type
class HasHomeHandler master where
    getHomeHandler :: HandlerFor master Html

-- Specifies the home page handler for our main application
instance HasHomeHandler App where
    getHomeHandler = getHomeR

-- Greets the user, creates a pinao interface with three octaves, and creates the music generator
getHomeR = defaultLayout $ do 
    -- Linking the stylesheet for the page
    addStylesheet $ StaticR styles_css
    [whamlet|
        By: Marina Seheon, Jack Vanlyssel, and Joesph Barela
        
        <div class="greeting">
            Hello, welcome to our Piano Music Generator!

        <div .piano>
            <button .white-key onclick="playNote('C3')">
            <button .black-key onclick="playNote('Cs3')">
            <button .white-key onclick="playNote('D3')">
            <button .black-key onclick="playNote('Ds3')">
            <button .white-key onclick="playNote('E3')">
            <button .white-key onclick="playNote('F3')">
            <button .black-key onclick="playNote('Fs3')">
            <button .white-key onclick="playNote('G3')">
            <button .black-key onclick="playNote('Gs3')">
            <button .white-key onclick="playNote('A3')">
            <button .black-key onclick="playNote('As3')">
            <button .white-key onclick="playNote('B3')">
            <button .white-key onclick="playNote('C4')">
            <button .black-key onclick="playNote('Cs4')">
            <button .white-key onclick="playNote('D4')">
            <button .black-key onclick="playNote('Ds4')">
            <button .white-key onclick="playNote('E4')">
            <button .white-key onclick="playNote('F4')">
            <button .black-key onclick="playNote('Fs4')">
            <button .white-key onclick="playNote('G4')">
            <button .black-key onclick="playNote('Gs4')">
            <button .white-key onclick="playNote('A4')">
            <button .black-key onclick="playNote('As4')">
            <button .white-key onclick="playNote('B4')">
            <button .white-key onclick="playNote('C5')">
            <button .black-key onclick="playNote('Cs5')">
            <button .white-key onclick="playNote('D5')">
            <button .black-key onclick="playNote('Ds5')">
            <button .white-key onclick="playNote('E5')">
            <button .white-key onclick="playNote('F5')">
            <button .black-key onclick="playNote('Fs5')">
            <button .white-key onclick="playNote('G5')">
            <button .black-key onclick="playNote('Gs5')">
            <button .white-key onclick="playNote('A5')">
            <button .black-key onclick="playNote('As5')">
            <button .white-key onclick="playNote('B5')">

        <div .music-settings>
            <form id="music-settings-form">
                <label for="timeSignature">Time Signature:
                <select id="timeSignature" name="timeSignature">
                    <option value="4/4">4/4
                    <option value="3/4">3/4
                    <option value="2/4">2/4
                    <option value="6/8">6/8
                <label for="bpm">BPM:
                <input type="number" id="bpm" name="bpm" value="120">
                <label for="keySignature">Key Signature:
                <select id="keySignature" name="keySignature">
                    <option value="C">C
                    <option value="G">G
                    <option value="D">D
                    <option value="A">A
                    <option value="E">E
                    <option value="B">B
                    <option value="Gb">Gb
                    <option value="Db">Db
                    <option value="Ab">Ab
                    <option value="Eb">Eb
                    <option value="Bb">Bb
                    <option value="F">F
                <button type="button" onclick="generateMusic()">Generate

        <script>
            document.addEventListener("DOMContentLoaded", function() {
                let lastPlayed = 0;
                const rateLimit = 200; // minimum time between note plays in milliseconds

                window.playNote = async function(note) {
                    const now = Date.now();
                    if (now - lastPlayed < rateLimit) {
                        console.log('Rate limited');
                        return;
                    }
                    lastPlayed = now;

                    try {
                        const response = await fetch(`/getPlayNoteR/${note}`);
                        if (!response.ok) {
                            throw new Error(`HTTP error! status: ${response.status}`);
                        }
                        const data = await response.text();
                        console.log(data);
                    } catch (error) {
                        console.error('Failed to fetch note:', error);
                    }
                }

                window.generateMusic = async function() {
                    const form = document.getElementById('music-settings-form');
                    const formData = new FormData(form);
                    const settings = Object.fromEntries(formData.entries());

                    try {
                        const response = await fetch(`/getGenerateMusicR?bpm=${settings.bpm}&timeSignature=${settings.timeSignature}&keySignature=${settings.keySignature}`);

                        if (response.ok) {
                        const text = await response.text();
                        console.log(text);  // You can update the page here if needed
                        } else {
                        console.error('Failed to fetch', response.status, response.statusText);
                        }

                    } catch (error) {
                    console.error('Fetch error:', error);
                    }
                }
            });
            
|]


