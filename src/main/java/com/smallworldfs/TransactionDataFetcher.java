package com.smallworldfs;

import com.smallworldfs.model.Transaction;
import com.smallworldfs.service.impl.TransactionService;
import lombok.RequiredArgsConstructor;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

@RequiredArgsConstructor
public class TransactionDataFetcher {

  private final TransactionService transactionService;

  /**
   * Returns the sum of the amounts of all transactions
   */
  public double getTotalTransactionAmount() {
    List<Transaction> transaction = transactionService.getAllTransaction();
    return transaction.isEmpty() ? 0.0 : transaction
        .stream()
        .distinct()
        .mapToDouble(Transaction::amount)
        .sum();
  }

  /**
   * Returns the sum of the amounts of all transactions sent by the specified client
   */
  public double getTotalTransactionAmountSentBy(String senderFullName) {
    List<Transaction> transaction = transactionService.getAllTransaction();
    return transaction.isEmpty() ? 0.0 : transaction
        .stream()
        .distinct()
        .filter(e -> senderFullName.equals(e.senderFullName()))
        .mapToDouble(Transaction::amount)
        .sum();
  }

  /**
   * Returns the highest transaction amount
   */
  public double getMaxTransactionAmount() {
    List<Transaction> transaction = transactionService.getAllTransaction();
    return transaction.isEmpty() ? 0.0 : transaction
        .stream()
        .distinct()
        .mapToDouble(Transaction::amount)
        .max()
        .orElse(0.0);
  }

  /**
   * Counts the number of unique clients that sent or received a transaction
   */
  public long countUniqueClients() {
    List<Transaction> transaction = transactionService.getAllTransaction();
    return transaction.isEmpty() ? 0 : transaction
        .stream()
        .distinct()
        .count();
  }

  /**
   * Returns whether a client (sender or beneficiary) has at least one transaction with a compliance
   * issue that has not been solved
   */
  public boolean hasOpenComplianceIssues(String clientFullName) {
    List<Transaction> transaction = transactionService.getAllTransaction();
    return transaction.isEmpty() ? Boolean.FALSE : transaction
        .stream()
        .anyMatch(e -> (e.senderFullName().equals(clientFullName) ||
            e.beneficiaryFullName().equals(clientFullName)) &&
            e.issueSolved().equals(Boolean.FALSE));
  }

  /**
   * Returns all transactions indexed by beneficiary name
   */
  public Map<String, List<Transaction>> getTransactionsByBeneficiaryName() {
    List<Transaction> transaction = transactionService.getAllTransaction();
    return transaction.isEmpty() ? Collections.emptyMap() : transaction
        .stream().collect(Collectors.groupingBy(Transaction::beneficiaryFullName));
  }

  /**
   * Returns the identifiers of all open compliance issues
   */
  public Set<Integer> getUnsolvedIssueIds() {
    List<Transaction> transaction = transactionService.getAllTransaction();
    return transaction.isEmpty() ? Collections.emptySet() : transaction
        .stream()
        .filter(e -> Boolean.FALSE.equals(e.issueSolved()) && e.issueId() != null)
        .map(Transaction::issueId)
        .collect(Collectors.toSet());
  }

  /**
   * Returns a list of all solved issue messages
   */
  public List<String> getAllSolvedIssueMessages() {
    List<Transaction> transaction = transactionService.getAllTransaction();
    return transaction.isEmpty() ? Collections.emptyList() : transaction
        .stream()
        .filter(e -> Boolean.TRUE.equals(e.issueSolved()) && !(e.issueMessage().isEmpty()))
        .map(Transaction::issueMessage)
        .toList();
  }

  /**
   * Returns the 3 transactions with the highest amount sorted by amount descending
   */
  public List<Transaction> getTop3TransactionsByAmount() {
    List<Transaction> transaction = transactionService.getAllTransaction();
    return transaction.isEmpty() ? Collections.emptyList() : transaction
        .stream()
        .distinct()
        .sorted(Comparator.comparingDouble(Transaction::amount).reversed())
        .limit(3)
        .toList();
  }

  /**
   * Returns the senderFullName of the sender with the most total sent amount
   */
  public Optional<String> getTopSender() {
    List<Transaction> transaction = transactionService.getAllTransaction();

    if (transaction.isEmpty())
      return Optional.empty();

    Map<String, Double> topSenderMap = transactionService.getAllTransaction()
        .stream().collect(Collectors.
            groupingBy(Transaction::senderFullName, Collectors.summingDouble(Transaction::amount)));
    return topSenderMap.entrySet().stream()
        .max(Comparator.comparingDouble(Map.Entry::getValue))
        .map(Map.Entry::getKey);
  }

}
