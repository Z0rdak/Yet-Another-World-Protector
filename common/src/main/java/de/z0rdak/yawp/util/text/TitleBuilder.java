package de.z0rdak.yawp.util.text;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import net.minecraft.ChatFormatting;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.ComponentUtils;
import net.minecraft.network.protocol.game.*;
import net.minecraft.server.level.ServerPlayer;
import org.jetbrains.annotations.NotNull;

public final class TitleBuilder {
    private static final int DEFAULT_FADE_IN = 10;
    private static final int DEFAULT_STAY = 80;
    private static final int DEFAULT_FADE_OUT = 20;

    private final ServerPlayer player;
    private final IProtectedRegion region;

    private Component title;
    private Component subtitle;
    private Component actionbar;
    private int fadeIn = DEFAULT_FADE_IN;
    private int stay = DEFAULT_STAY;
    private int fadeOut = DEFAULT_FADE_OUT;
    private boolean customTimings = false;

    private TitleBuilder(@NotNull ServerPlayer player, @NotNull IProtectedRegion region) {
        this.player = player;
        this.region = region;
    }

    public static TitleBuilder of(@NotNull ServerPlayer player, @NotNull IProtectedRegion region) {
        return new TitleBuilder(player, region);
    }

    public TitleBuilder title(@NotNull Component title) {
        this.title = title;
        return this;
    }

    public TitleBuilder subtitle(Component subtitle) {
        this.subtitle = subtitle;
        return this;
    }

    public TitleBuilder actionbar(Component actionbar) {
        this.actionbar = actionbar;
        return this;
    }

    public TitleBuilder fadeIn(int ticks) {
        this.fadeIn = ticks;
        this.customTimings = true;
        return this;
    }

    public TitleBuilder stay(int ticks) {
        this.stay = ticks;
        this.customTimings = true;
        return this;
    }

    public TitleBuilder fadeOut(int ticks) {
        this.fadeOut = ticks;
        this.customTimings = true;
        return this;
    }

    public TitleBuilder timings(int fadeIn, int stay, int fadeOut) {
        this.fadeIn = fadeIn;
        this.stay = stay;
        this.fadeOut = fadeOut;
        this.customTimings = true;
        return this;
    }

    /** Sets title to the region’s name (yellow). */
    public TitleBuilder titleRegionName() {
        this.title = Component.translatable("%s", region.getName()).withStyle(ChatFormatting.YELLOW);
        return this;
    }

    /** Sets subtitle to “Welcome to {region}, {player}” (gold). */
    public TitleBuilder subtitleWelcome() {
        this.subtitle = Component.translatable("Welcome to %s, %s!", region.getName(), player.getScoreboardName())
                .withStyle(ChatFormatting.GOLD);
        return this;
    }

    public TitleBuilder subtitleBye() {
        this.subtitle = Component.translatable("No leaving %s, bye %s!", region.getName(), player.getScoreboardName())
                .withStyle(ChatFormatting.GOLD);
        return this;
    }

    public BuiltTitle build() {
        if (title == null)
            throw new IllegalStateException("Title needs to be present.");
        return new BuiltTitle(player, title, subtitle, actionbar, fadeIn, stay, fadeOut, customTimings);
    }

    public void send() {
        build().send();
    }


    public static final class BuiltTitle {
        private final ServerPlayer player;
        private final Component title;
        private final Component subtitle;
        private final Component actionbar;
        private final int fadeIn, stay, fadeOut;
        private final boolean customTimings;

        private BuiltTitle(ServerPlayer player, Component title, Component subtitle,
                           Component actionbar, int fadeIn, int stay, int fadeOut, boolean customTimings) {
            this.player = player;
            this.title = title;
            this.subtitle = subtitle;
            this.actionbar = actionbar;
            this.fadeIn = fadeIn;
            this.stay = stay;
            this.fadeOut = fadeOut;
            this.customTimings = customTimings;
        }

        public void sendTo(ServerPlayer player) {
            try {
                var source = player.createCommandSourceStack();
                var conn = player.connection;

                conn.send(new ClientboundClearTitlesPacket(true));
                if (customTimings)
                    conn.send(new ClientboundSetTitlesAnimationPacket(fadeIn, stay, fadeOut));
                conn.send(new ClientboundClearTitlesPacket(false));

                if (title != null) {
                    var resolved = ComponentUtils.updateForEntity(source, title, player, 0);
                    conn.send(new ClientboundSetTitleTextPacket(resolved));
                }

                if (subtitle != null) {
                    var resolved = ComponentUtils.updateForEntity(source, subtitle, player, 0);
                    conn.send(new ClientboundSetSubtitleTextPacket(resolved));
                }

                if (actionbar != null) {
                    var resolved = ComponentUtils.updateForEntity(source, actionbar, player, 0);
                    conn.send(new ClientboundSetActionBarTextPacket(resolved));
                }

            } catch (CommandSyntaxException e) {
                throw new RuntimeException("Failed to send title for player: " + player.getName().getString(), e);
            }
        }
        public void send() {
            this.sendTo(player);
        }
    }
}
