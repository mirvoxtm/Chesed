![image](logo.png)
_________________________________________
**Chesed** is a simple Visual Novel/ADV Game Engine built entirely in Haskell with gloss and megaparsec.
**Chesed** is currently in early development and features a simple scripting language that you can use to make your own novels.

## File Structure
Chesed has two files. The main file (chesed.exe) and a *game* folder.
The *game* folder is used for anything related to the game itself, such as the script (located in *script.chesed*), the images folder and the gui folder.

It is obligatory that this folder structure does not be tampered with. For example, when placing in a background with the **(bg)** tag, the specified image must be inside the *game/images* folder, or else it wont load.

## Scripting
The scripting in Chesed is made to be simple. Here's an example:
```
    (bg | background.png)
    Narrator "Suddenly, a girl approaches me, she seems a bit clumsy..."
    (show | Hod | 0 | 0 | 0.64 | 0.64)dummy.png(/show)
    Hod "Hello! My name is Hod, Welcome to Chesed!"
    (show | Hod | 0 | 0 | 0.64 | 0.64)dummy_happy.png(/show)
    Hod "Chesed is a simple Visual-Novel/ADV Game Engine made entirely in Haskell! Would you look at that!"
    Narrator "Once you add your images and backgrounds, you can just write away!"
```

Here, **Narrator** is a reserved word that does not show the character's name. The **(bg)** tag adds a background to the current scene. The **(show)** tag shows a sprite on the screen in a certain **x** and **y** position, as well as **width and height scaling**.