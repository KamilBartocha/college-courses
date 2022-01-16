package com.example;

import com.example.model.order.Instrument;
import com.example.model.order.Offer;
import com.example.model.rest.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Random;
import java.util.UUID;

import static java.lang.Math.min;

public class OrdersController implements Runnable {

    private static final long minQty = 10;
    private static final long minAsk = 13;
    private static final long minBid = 79;
    private static long lastBid = 79;
    private static long lastSel = 13;

    private static final Logger logger = LoggerFactory.getLogger(OrdersController.class);

    private final Platform marketPlugin;

    private final Random rg = new Random();

    public OrdersController(Platform marketPlugin) {
        this.marketPlugin = marketPlugin;
    }

    public static long min(long a, long b, long c) {
        return Math.min(Math.min(a, b), c);
    }

    @Override
    public void run() {
        final var fetchedInstruments = marketPlugin.instruments();
        final var fetchedPortfolio = marketPlugin.portfolio();

        if (fetchedPortfolio instanceof PortfolioResponse.Other other) {
            logger.error("portfolio call does not return portfolio {}", other);
        }

        if (fetchedPortfolio instanceof PortfolioResponse.Portfolio portfolio && fetchedInstruments instanceof InstrumentsResponse.Instruments instruments) {
            portfolio
                    .portfolio()
                    .stream()
                    .map(PortfolioResponse.Portfolio.Element::instrument)
                    .forEach(instrument -> {
                        final var orders = marketPlugin.orders(new OrdersRequest(instrument));
                        logger.info("instrument {} has orders {}", instrument, orders);
                    });

            final var selectedForBuy = instruments
                    .available()
                    .stream()
                    .filter(pe -> {
                        final var history = marketPlugin.history(new HistoryRequest(pe));
                        if (history instanceof HistoryResponse.History correct) {
                            try {
                                long avgBoughtPrice;
                                avgBoughtPrice = (long) correct
                                        .bought()
                                        .stream()
                                        .mapToLong(b -> b.offer().price())
                                        .average()
                                        .orElse(minBid);
                                var lastBoughtPrice = correct
                                        .bought()
                                        .stream()
                                        .findFirst()
                                        .stream()
                                        .toList()
                                        .get(0)
                                        .offer()
                                        .price();
                                lastBid = lastBoughtPrice;
                                return avgBoughtPrice > lastBoughtPrice;
                            } catch (Exception emptyHistory) {
                                return false;
                            }
                        } else {
                            return false;
                        }
                    })
                    .toList();
            logger.info("instruments to buy {}", selectedForBuy);
            selectedForBuy
                    .stream()
                    .map(pe -> {
                        final var history = marketPlugin.history(new HistoryRequest(pe));

                        long bid;
                        long bidAvg;
                        if (history instanceof HistoryResponse.History correct) {

                            bidAvg = (long) (
                                    1 * correct
                                            .bought()
                                            .stream()
                                            .mapToLong(b -> b.offer().price())
                                            .average()
                                            .orElse(minBid)
                            );
                        } else {
                            bidAvg = minBid;
                        }
//                        System.out.println(lastBid + "  " + minBid + "  " + bidAvg);
                        bid = Math.min(lastBid + 1, bidAvg);
//                        System.out.println("bid final: " + bid);
                        final var qty = rg.nextInt((int) (portfolio.cash() / 10 / bid));
                        final var buy = new SubmitOrderRequest.Buy(pe.symbol(), UUID.randomUUID().toString(), qty, bid);

                        logger.info("order to submit {}", buy);
                        return buy;
                    });
//                    .map(marketPlugin::buy)
//                    .forEach(vo -> logger.info("order placed with response {}", vo));

            // All portfolio instruments:
            final var selectedForSell = portfolio
                    .portfolio()
                    .stream()
//                    .filter(element -> element.qty() > 500)
                    .toList();

            selectedForSell
                    .stream()
                    .map(pe -> {
                        logger.info("portfolio element {}", pe);
                        final var history = marketPlugin.history(new HistoryRequest(pe.instrument()));
                        long sellPrice;
                        long avgAskPrice;
                        if (history instanceof HistoryResponse.History correct) {
                            avgAskPrice = (long) (
                                    1 * correct
                                            .sold()
                                            .stream()
                                            .mapToLong(b -> b.offer().price())
                                            .average()
                                            .orElse(minAsk)
                            );
                            var lastSoldPrice = correct
                                    .sold()
                                    .stream()
                                    .findFirst()
                                    .stream()
                                    .toList()
                                    .get(0)
                                    .offer()
                                    .price();
                            lastSel = lastSoldPrice;
                        } else {
                            avgAskPrice = minAsk;
                        }
                        sellPrice = Math.max(Math.min(avgAskPrice, lastSel), minAsk);
                        sellPrice = (long) (sellPrice * 1.05);
                        logger.info("sell price {}", sellPrice);
                        final long qty = Math.min(pe.qty(), minQty);
                        final var sell = new SubmitOrderRequest.Sell(pe.instrument().symbol(), UUID.randomUUID().toString(), qty, sellPrice);

                        logger.info("order to submit {}", sell);
                        return sell;
                    });
//                    .map(marketPlugin::sell)
//                    .forEach(vo -> logger.info("order placed with response {}", vo));
        }
    }
}
