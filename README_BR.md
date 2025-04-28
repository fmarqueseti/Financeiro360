# 💰 Financeiro360

Sistema simples de controle financeiro pessoal, utilizando IBM COBOL e arquivos para armazenar dados de contas e transações.
Projeto desenvolvido como parte do desafio [Edu360](https://www.instagram.com/p/DI3xL_Hx02H).

---

## 📋 Funcionalidades

- Cadastro de contas financeiras (banco, carteira, etc.).
- Registro de transações (crédito e débito).
- Geração de extrato por conta.
- Cálculo automático de saldo atualizado.
- Armazenamento de dados em arquivos.

---

## 💂️ Estrutura de Arquivos

- `CADCONTA` – Contém a lista de contas cadastradas.
- `CADTRANS` – Contém todas as transações realizadas.

Formato dos registros:

**CADCONTA**
```
IDCONTA|NOME|SALDOINI
```

**CADTRANS**
```
DATA|TIPO|VALOR|DESCR|IDCONTA
```

---

## 🗕️ Status do Projeto

✅ Em fase final de concepção e desenvolvimento.

---

## 🤝 Desenvolvimento Colaborativo

Este projeto está sendo desenvolvido em conjunto pelos integrantes do canal **#educ-360** do servidor Discord **[Mainframe Club](https://discord.com/invite/BuAPCKm9sf)**.

### 👥 Integrantes

- Fábio Marques [Perfil LinkedIn](https://www.linkedin.com/in/fmrqs/)
- Thiago Souza [Perfil LinkedIn](https://www.linkedin.com/in/thiago-o-souza/)

---

## 📜 Licença

Este projeto está licenciado sob a Licença MIT.
Consulte o arquivo [LICENSE](https://github.com/fmarqueseti/Financeiro360/blob/main/LICENSE) para mais detalhes.

---

**Desenvolvido para o desafio [Edu360](https://www.instagram.com/p/DI3xL_Hx02H) 🚀.**

