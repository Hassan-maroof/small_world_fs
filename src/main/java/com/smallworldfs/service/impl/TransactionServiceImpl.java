package com.smallworldfs.service.impl;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.smallworldfs.model.Transaction;
import org.springframework.beans.factory.annotation.Value;

import java.io.IOException;
import java.util.List;

public class TransactionServiceImpl implements TransactionService {

  @Value("${datasource.json.source}")
  private String jsonSourceFile;

  @Override
  public List<Transaction> getAllTransaction() throws IOException {

    ObjectMapper objectMapper = new ObjectMapper();
    return objectMapper.readValue(jsonSourceFile,
        new TypeReference<List<Transaction>>() {
        });
  }
}
