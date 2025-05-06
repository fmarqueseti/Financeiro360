# 💰 Financeiro360

A simple personal finance management system developed using IBM COBOL and file-based storage for accounts and transactions.  
This project is part of the [Edu360](https://www.instagram.com/p/DI3xL_Hx02H) challenge.

---

## 📋 Features

- Register financial accounts (bank accounts, wallets, etc.).
- Record transactions (credits and debits).
- Generate account statements.
- Automatically calculate updated balances.
- Store data using sequential files.

---

## 💂️ File Structure

- `COB` - Contains the COBOL source files.
- `CPY` - Contains the COPYBOOKS of VSAM files.
- `JCL` - Contains the JCL files.

VSAM files record formats:

**CADCONTA**
```
IDCONTA|NAME|INITIAL_BALANCE
```

**CADTRANS**
```
DATE|TYPE|AMOUNT|DESCRIPTION|ACCOUNT_ID
```

---

## 🗕️ Project Status

✅ Currently in the final stages of design and development.

---

## 🤝 Collaborative Development

This project is being collaboratively developed by members of the **#educ-360** channel in the **[Mainframe Club Discord Server](https://discord.com/invite/BuAPCKm9sf)**.

### 👥 Contributors

- Fábio Marques [LinkedIn Profile](https://www.linkedin.com/in/fmrqs/)
- Thiago Souza [LinkedIn Profile](https://www.linkedin.com/in/thiago-o-souza/)

---

## 📜 License

This project is licensed under the MIT License.  
Please refer to the [LICENSE](https://github.com/fmarqueseti/Financeiro360/blob/main/LICENSE) file for more details.

---

**Developed for the [Edu360](https://www.instagram.com/p/DI3xL_Hx02H) challenge 🚀.**

