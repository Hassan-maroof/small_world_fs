package com.smallworldfs.service.impl;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.smallworldfs.exception.Error;
import com.smallworldfs.exception.ServiceException;
import com.smallworldfs.model.Transaction;
import jakarta.annotation.PostConstruct;
import org.springframework.beans.factory.annotation.Value;

import java.util.List;

public class TransactionServiceImpl implements TransactionService {

  @Value("${datasource.json.source}")
  private String jsonSourceFile;
  private static List<Transaction> transactions;

  @Override
  public List<Transaction> getAllTransaction() {
    return transactions;
  }

  @PostConstruct
  private void loadTransactions() {
    try {
      ObjectMapper objectMapper = new ObjectMapper();
      transactions = objectMapper.readValue(jsonSourceFile, new TypeReference<List<Transaction>>() {
      });
    } catch (Exception e) {
      throw new ServiceException(Error.FILE_NOT_FOUND, e);
    }
  }

}
