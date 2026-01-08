
import { describe, expect, it } from "vitest";
import { Cl, cvToValue } from "@stacks/transactions";

const accounts = simnet.getAccounts();
const deployer = accounts.get("deployer")!;
const alice = accounts.get("wallet_1")!;
const bob = accounts.get("wallet_2")!;

const setMinterToFluxVerse = () =>
  simnet.callPublicFn(
    "fluxtoken",
    "set-minter",
    [Cl.contractPrincipal(deployer, "fluxVerse")],
    deployer,
  );

const readVault = (owner: string) =>
  simnet.callReadOnlyFn("fluxVerse", "get-vault", [Cl.principal(owner)], owner).result;

describe("fluxVerse core flows", () => {
  it("runs the vault lifecycle with borrow and repay", () => {
    const { result: setMinterResult } = setMinterToFluxVerse();
    expect(setMinterResult).toBeOk(Cl.bool(true));

    const { result: openResult } = simnet.callPublicFn("fluxVerse", "open-vault", [], alice);
    expect(openResult).toBeOk(Cl.bool(true));

    const { result: depositResult } = simnet.callPublicFn(
      "fluxVerse",
      "deposit-collateral",
      [Cl.uint(2_000_000)],
      alice,
    );
    expect(depositResult).toBeOk(Cl.bool(true));
    expect(readVault(alice)).toBeSome(expect.anything());

    const { result: borrowResult } = simnet.callPublicFn(
      "fluxVerse",
      "borrow",
      [Cl.uint(500_000)],
      alice,
    );
    expect(borrowResult).toBeOk(Cl.bool(true));

    const { result: repayResult } = simnet.callPublicFn(
      "fluxVerse",
      "repay",
      [Cl.uint(200_000)],
      alice,
    );
    expect(repayResult).toBeOk(Cl.uint(200_000));

    const { result: withdrawResult } = simnet.callPublicFn(
      "fluxVerse",
      "withdraw-collateral",
      [Cl.uint(500_000)],
      alice,
    );
    expect(withdrawResult).toBeOk(Cl.bool(true));

    const { result: repayAllResult } = simnet.callPublicFn(
      "fluxVerse",
      "repay",
      [Cl.uint(400_000)],
      alice,
    );
    expect(repayAllResult).toBeOk(Cl.uint(300_000));

    const { result: closeResult } = simnet.callPublicFn("fluxVerse", "close-vault", [], alice);
    expect(closeResult).toBeOk(Cl.uint(1_500_000));
    expect(readVault(alice)).toBeNone();
  });

  it("liquidates an undercollateralized vault after a price drop", () => {
    const { result: setMinterResult } = setMinterToFluxVerse();
    expect(setMinterResult).toBeOk(Cl.bool(true));

    const { result: openResult } = simnet.callPublicFn("fluxVerse", "open-vault", [], bob);
    expect(openResult).toBeOk(Cl.bool(true));

    const { result: depositResult } = simnet.callPublicFn(
      "fluxVerse",
      "deposit-collateral",
      [Cl.uint(2_000_000)],
      bob,
    );
    expect(depositResult).toBeOk(Cl.bool(true));

    const { result: borrowResult } = simnet.callPublicFn(
      "fluxVerse",
      "borrow",
      [Cl.uint(1_300_000)],
      bob,
    );
    expect(borrowResult).toBeOk(Cl.bool(true));

    const { result: priceResult } = simnet.callPublicFn(
      "fluxVerse",
      "update-stx-price",
      [Cl.uint(800_000)],
      deployer,
    );
    expect(priceResult).toBeOk(Cl.bool(true));

    const { result: liquidatableResult } = simnet.callReadOnlyFn(
      "fluxVerse",
      "is-vault-liquidatable",
      [Cl.principal(bob)],
      bob,
    );
    expect(liquidatableResult).toBeOk(Cl.bool(true));

    const { result: mintLiquidator } = simnet.callPublicFn(
      "fluxtoken",
      "mint-for-testing",
      [Cl.uint(1_400_000)],
      deployer,
    );
    expect(mintLiquidator).toBeOk(Cl.bool(true));

    const { result: liquidateResult } = simnet.callPublicFn(
      "fluxVerse",
      "liquidate",
      [Cl.principal(bob)],
      deployer,
    );
    expect(liquidateResult).toBeOk(expect.anything());
    expect(readVault(bob)).toBeNone();
  });

  it("executes governance proposals after voting and quorum", () => {
    const { result: mintProposer } = simnet.callPublicFn(
      "fluxtoken",
      "mint-for-testing",
      [Cl.uint(300_000)],
      alice,
    );
    expect(mintProposer).toBeOk(Cl.bool(true));

    const { result: mintVoter } = simnet.callPublicFn(
      "fluxtoken",
      "mint-for-testing",
      [Cl.uint(300_000)],
      bob,
    );
    expect(mintVoter).toBeOk(Cl.bool(true));

    const { result: proposeResult } = simnet.callPublicFn(
      "fluxVerse",
      "propose",
      [Cl.stringAscii("min-collateral-ratio"), Cl.uint(160)],
      alice,
    );
    expect(proposeResult).toBeOk(expect.anything());

    const proposalId = BigInt(cvToValue(proposeResult).value);

    const { result: voteProposer } = simnet.callPublicFn(
      "fluxVerse",
      "vote",
      [Cl.uint(proposalId), Cl.bool(true), Cl.uint(300_000)],
      alice,
    );
    expect(voteProposer).toBeOk(Cl.bool(true));

    const { result: voteVoter } = simnet.callPublicFn(
      "fluxVerse",
      "vote",
      [Cl.uint(proposalId), Cl.bool(true), Cl.uint(300_000)],
      bob,
    );
    expect(voteVoter).toBeOk(Cl.bool(true));

    simnet.mineEmptyBlocks(1_100);

    const { result: executeResult } = simnet.callPublicFn(
      "fluxVerse",
      "execute-proposal",
      [Cl.uint(proposalId)],
      alice,
    );
    expect(executeResult).toBeOk(Cl.bool(true));

    const { result: paramsResult } = simnet.callReadOnlyFn(
      "fluxVerse",
      "get-system-parameters",
      [],
      alice,
    );
    expect(paramsResult).toBeTuple({
      "interest-rate-bp": Cl.uint(500),
      "liquidation-bonus": Cl.uint(10),
      "liquidation-ratio": Cl.uint(130),
      "min-collateral-ratio": Cl.uint(160),
    });
  });
});
