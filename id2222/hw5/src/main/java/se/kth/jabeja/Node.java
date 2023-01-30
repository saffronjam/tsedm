package se.kth.jabeja;

import java.util.ArrayList;

public class Node {

    private final int id;
    private int color;
    private final int initColor;
    private final ArrayList<Integer> neighbours;

    public Node(int id, int color) {
        this.id = id;
        this.color = color;
        this.initColor = color;
        this.neighbours = new ArrayList<>();
    }

    public int getId() {
        return this.id;
    }

    public int getColor() {
        return this.color;
    }

    public void setColor(int color) {
        this.color = color;
    }

    public int getDegree() {
        return this.neighbours.size();
    }

    public int getInitColor() {
        return this.initColor;
    }

    public ArrayList<Integer> getNeighbours() {
        return this.neighbours;
    }

    public void setNeighbours(ArrayList<Integer> neighbours) {
        this.neighbours.addAll(neighbours);
    }

    @Override
    public String toString() {
        return "id: " + id + ", color: " + color + ", neighbours: " + neighbours + "\n";
    }
}
