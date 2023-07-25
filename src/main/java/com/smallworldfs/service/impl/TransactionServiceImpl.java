package com.smallworldfs.service.impl;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.smallworldfs.exception.Error;
import com.smallworldfs.exception.ServiceException;
import com.smallworldfs.model.Transaction;
import jakarta.annotation.PostConstruct;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@Slf4j
public class TransactionServiceImpl implements TransactionService {

  @Value("${datasource.json.source}")
  private String jsonSourceFile;
  private static List<Transaction> transactions;

  /**
   * Loading json file and storing it in static variable -> transactions
   **/
  @PostConstruct
  void initTransaction() {
    loadTransactions();
  }

  @Override
  public List<Transaction> getAllTransaction() {
    return transactions;
  }

  private void loadTransactions() {
    try {
      ObjectMapper objectMapper = new ObjectMapper();
      log.info("Start :: loadTransactions() reading and loading data from file");
      transactions = objectMapper.readValue(jsonSourceFile, new TypeReference<List<Transaction>>() {
      });
      log.info("Ends :: loadTransactions() reading and loading data from file");
    } catch (Exception e) {
      throw new ServiceException(Error.FILE_NOT_FOUND, e);
    }
  }

}
