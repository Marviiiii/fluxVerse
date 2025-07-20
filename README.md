# FluxVerse CDP System

FluxVerse is a local development mockup of a **Collateralized Debt Position (CDP) system** for Stacks (STX) and FluxToken (FLUX), built with [Clarity](https://docs.stacks.co/docs/clarity-language/overview/). It allows users to deposit STX as collateral and borrow FLUX tokens, with automated interest accrual, liquidation, and admin controls.

---

## Features

- **SIP-010 Compliant Token**: FLUX token contract implements standard transfer, approve, mint, and burn functions.
- **Vaults**: Each user can open a vault to deposit STX and borrow FLUX.
- **Interest Accrual**: Debt increases over time based on a fixed interest rate.
- **Collateral Ratio Enforcement**: Minimum collateral ratio (150%) and liquidation threshold (130%) are enforced.
- **Liquidation**: Under-collateralized vaults can be liquidated by anyone for a bonus.
- **Admin Controls**: Owner can update prices and transfer contract ownership.
- **Testing Utilities**: Includes functions for minting tokens for local testing.

---

## Contracts

### 1. `fluxtoken.clar`

Implements the FLUX token:

- **Standard SIP-010 functions**: `transfer`, `approve`, `transfer-from`, `get-balance`, `get-total-supply`, etc.
- **Mint/Burn**: Only the contract owner (initially deployer, can be set to CDP contract) can mint/burn tokens.
- **Testing Utility**: Anyone can mint tokens for testing with `mint-for-testing`.
- **Admin**: Owner can transfer ownership.

### 2. fluxVerse.clar

Implements the CDP system:

- **Vault Management**: `open-vault`, `deposit-collateral`, `withdraw-collateral`, `close-vault`.
- **Borrow/Repay**: `borrow` mints FLUX to user, `repay` burns FLUX from user.
- **Interest Calculation**: Interest accrues per block.
- **Liquidation**: `liquidate` allows anyone to repay an under-collateralized vault and seize collateral plus bonus.
- **Admin Functions**: `set-contract-owner`, `update-stx-price`, `update-flux-price`.
- **Read-only Views**: For vault info, system info, and liquidation status.

---

## Getting Started

### Prerequisites

- [Clarinet](https://docs.hiro.so/clarinet/get-started) (for local development)
- [Stacks Blockchain](https://docs.stacks.co/docs/intro/overview/)

### Setup

1. Clone this repository.
2. Open in [VS Code](https://code.visualstudio.com/).
3. Use Clarinet to test and deploy contracts locally.

### Example Workflow

1. **Open a Vault**  
   Call `open-vault` to create your vault.

2. **Deposit Collateral**  
   Call `deposit-collateral` with STX amount.

3. **Borrow FLUX**  
   Call `borrow` with desired FLUX amount (must maintain minimum collateral ratio).

4. **Repay Debt**  
   Call `repay` with FLUX amount to reduce debt.

5. **Withdraw Collateral**  
   Call `withdraw-collateral` (only if collateral ratio is safe).

6. **Liquidate Vault**  
   If a vault falls below the liquidation ratio, anyone can call `liquidate` to seize collateral.

---

## Parameters

- **Minimum Collateral Ratio**: 150%
- **Liquidation Ratio**: 130%
- **Liquidation Bonus**: 10%
- **Interest Rate**: 5% annualized
- **Prices**: Set by contract owner (for local testing)

---

## Security Notes

- This implementation is for **local development and testing only**.
- Price feeds are set manually; in production, use a secure oracle.
- Always audit smart contracts before deploying to mainnet.

---

## License

MIT License

---

## Authors

- Marvellous Madaki

---

## File Structure

```
contracts/
  fluxtoken.clar      # SIP-010 FLUX token contract
  fluxVerse.clar      # CDP system contract
```

---

## Useful Links

- [Clarity Language Docs](https://docs.stacks.co/docs/clarity-language/overview/)
- [SIP-010 Fungible Token Standard](https://github.com/stacksgov/sips/blob/main/sips/sip-010/sip-010-fungible-token-standard.md)
- [Clarinet Testing](https://docs.hiro.so/clarinet/testing)

---

## Contact

For questions or collaboration, open an issue or reach out via GitHub.

---

**Happy hacking with FluxVerse!**
