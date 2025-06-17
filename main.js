const {app,BrowserWindow}=require('electron');
const path=require('path');
const sqlite3=require('sqlite3').verbose();
const {spawn}=require('child_process');
const fs=require('fs');

const port=4574;  // Port for the Shiny App

// Define ProgramData path for database storage
const programDataPath=path.join(process.env.PROGRAMDATA,"local_SQL_storage_electron");
const dbPath=path.join(programDataPath,"SQL_storage.db");

// Ensure ProgramData directory exists
if(!fs.existsSync(programDataPath)) {
  fs.mkdirSync(programDataPath,{recursive: true});
}

// Verify R Portable exists
let execPath=path.join(app.getAppPath(),"R-Portable","App","R-Portable","bin","Rscript.exe").replace(/\\/g,"/");
let appPath=path.join(app.getAppPath(),"app.R").replace(/\\/g,"/");

if(!fs.existsSync(execPath)) {
  console.error(`R Portable not found at ${execPath}. Ensure it's bundled with the app.`);
  process.exit(1);
}

// Launch Shiny App
const childProcess=spawn(execPath,["-e",`shiny::runApp(file.path('${appPath}'), port=${port})`]);
childProcess.stdout.on('data',(data) => {
  console.log(`Shiny App Output: ${data}`);
});
childProcess.stderr.on('data',(data) => {
  console.error(`Error running R Script: ${data}`);
});

// Connect SQLite database
let db=new sqlite3.Database(dbPath,(err) => {
  if(err) {
    console.error('Error opening database:',err.message);
  } else {
    console.log('Connected to SQLite database at:',dbPath);

    // Ensure tables exists
    db.serialize(() => {
      // Subject table
      db.run(`
    CREATE TABLE IF NOT EXISTS subject (
      pk_subject TEXT PRIMARY KEY,
      name_subject TEXT NOT NULL,
      description_subject TEXT
    )
  `);

      // Storage table
      db.run(`
    CREATE TABLE IF NOT EXISTS storage (
      pk_storage TEXT PRIMARY KEY,
      description_storage TEXT,
      date_storage TEXT,
      fk_subject TEXT NOT NULL,
      fk_sample TEXT,
      FOREIGN KEY (fk_subject) REFERENCES subject(pk_subject) ON DELETE RESTRICT ON UPDATE CASCADE,
      FOREIGN KEY (fk_sample) REFERENCES sample(pk_sample) ON DELETE RESTRICT ON UPDATE CASCADE
    )
  `);

      // Sample table
      db.run(`
    CREATE TABLE IF NOT EXISTS sample (
      pk_sample TEXT PRIMARY KEY,
      valueName TEXT,
      numValue REAL,
      numValue2 REAL,
      numValue3 REAL,
      charValue TEXT,
      charValue2 TEXT,
      charValue3 TEXT,
      fk_storage TEXT NOT NULL,
      FOREIGN KEY (fk_storage) REFERENCES storage(pk_storage) ON DELETE RESTRICT ON UPDATE CASCADE
    )
  `);

      // Users table (passwords hashed)
      db.run(`
    CREATE TABLE IF NOT EXISTS users (
      username TEXT PRIMARY KEY,
      password_hash TEXT NOT NULL,
      display_name TEXT,
      created_at TEXT DEFAULT CURRENT_TIMESTAMP
    )
  `);

      // User settings
      db.run(`
    CREATE TABLE IF NOT EXISTS settings (
      username TEXT,
      key TEXT,
      value TEXT,
      PRIMARY KEY (username, key),
      FOREIGN KEY (username) REFERENCES users(username)
    )
  `);

      console.log("✅ All tables ensured.");
    });
  }
});

// Launch Electron Window
let mainWindow;
app.on('ready',() => {
  mainWindow=new BrowserWindow({width: 800,height: 600});
  mainWindow.loadURL(`http://127.0.0.1:${port}/`);
});